#' Retrieve Outbound Messages from Postmark
#'
#' Fetches a list of outbound messages from the Postmark API. This function
#' allows you to retrieve messages with pagination support and additional
#' filtering options.
#'
#' @param count An integer specifying the number of messages to retrieve.
#'   Must be positive and not exceed 500.
#' @param offset An integer specifying the number of messages to skip.
#'   Defaults to 0. The sum of count and offset must not exceed 10,000.
#' @param token Astring token. Check Postmark's API documentation. Can be set
#'  as `POSTMARK_SERVER_TOKEN` as an environment variable.
#' @param ... Additional query parameters to filter results.
#'   See <https://postmarkapp.com/developer/api/messages-api#outbound-messages>
#'   for supported parameters (e.g., `recipient`, `tag`, `status`).
#'
#' @returns A list of messages.
#' @export
#'
#' @examples
#' \dontrun{
#' # Get the 10 most recent outbound messages
#' recent_messages <- get_outbound_messages(10L)
#'
#' # Skip the first 50 messages and get the next 20
#' next_page <- get_outbound_messages(20L, offset = 50L)
#'
#' # Filter messages by recipient
#' to_john <- get_outbound_messages(
#'   count = 25L,
#'   recipient = "john@example.com"
#' )
#'
#' # Filter by tag and status
#' newsletter_sent <- get_outbound_messages(
#'   count = 100L,
#'   tag = "newsletter",
#'   status = "sent"
#' )
#'}
get_outbound_messages <- function(count, offset = 0L, token = NULL,  ...) {

  if (is.null(token)) {
    token <- get_token()
  }

  stopifnot(rlang::is_character(token, 1L))

  req <- build_req(count, offset, ...)
  header <- build_header(req, token)
  resp <- httr2::req_perform(header)
  out <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  out
}

build_req <- function(count, offset, ...) {

  stopifnot(
    rlang::is_integer(count, n = 1L),
    rlang::is_integer(offset, n = 1L),
    count > 0,
    count <= 500,
    count + offset <= 1e4
  )

  dots <- rlang::list2(...)

  # TODO: validate dots looking at the postmark API docs
  args <- rlang::names2(dots)

  if (!all(args %in% supported_args())) {
    rlang::abort(
      "Some arguments are not supported.",
      class = "not_supported_args"
    )
  }

  req <-
    httr2::request("https://api.postmarkapp.com") |>
    httr2::req_url_path_append("messages") |>
    httr2::req_url_path_append("outbound") |>
    httr2::req_url_query(count = count) |>
    httr2::req_url_query(offset = offset)

  if (!length(dots)) {
    return(req)
  }

  httr2::req_url_query(req, !!!dots)

}

build_header <- function(req, token) {
  req |>
    httr2::req_headers(
      "Accept" = "application/json",
      "X-Postmark-Server-Token" = token
    )
}
