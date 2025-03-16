#' Retrieve Outbound Messages from Postmark
#'
#' Fetches a list of outbound messages from the Postmark API. This function
#' allows you to retrieve messages with pagination support and additional
#' filtering options.
#'
#' @param count An integer specifying the number of messages to retrieve. Must be
#'  positive and not exceed 500.
#' @param offset An integer specifying the number of messages to skip. Defaults
#'  to 0. The sum of count and offset must not exceed 10,000.
#' @param token Astring token. Check  Can be set as `POSTMARK_SERVER_TOKEN` as an
#'  environment variable.
#' @param token Character string. Your Postmark API token. If NULL (default), the
#'  function will attempt to retrieve the token using `get_token()`
#'  `POSTMARK_SERVER_TOKEN` as an environment variable. Check the Postmark's API
#'  documentation on how to get a server token.
#' @param ... Additional query parameters to filter results. See
#'  <https://postmarkapp.com/developer/api/messages-api#outbound-messages> for
#'  supported parameters (e.g., `recipient`, `tag`, `status`).
#'
#' @returns A list of messages.
#' @export
#'
#' @examples
#' \dontrun{
#' # Get the 10 most recent outbound messages
#' recent_messages <- outbound_messages_fetch(10L)
#'
#' # Skip the first 50 messages and get the next 20
#' next_page <- outbound_messages_fetch(20L, offset = 50L)
#'
#' # Filter messages by recipient
#' to_john <- outbound_messages_fetch(
#'   count = 25L,
#'   recipient = "john@example.com"
#' )
#'
#' # Filter by tag and status
#' newsletter_sent <- outbound_messages_fetch(
#'   count = 100L,
#'   tag = "newsletter",
#'   status = "sent"
#' )
#'}
outbound_messages_fetch <- function(count, offset = 0L, token = NULL,  ...) {

  stopifnot(
    rlang::is_integer(count, n = 1L),
    rlang::is_integer(offset, n = 1L),
    count > 0,
    count <= 500,
    count + offset <= 1e4
  )

  req <- build_req("messages/outbound", "GET", token = token, count = count, offset = offset)
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = TRUE)

}

#' Collect all outbound email messages
#'
#' This function retrieves all outbound messages by making multiple API calls as
#' needed. It handles pagination automatically by generating appropriate batches
#' of count and offset values.
#'
#' @inheritParams outbound_messages_fetch
#' @param quiet Logical. If FALSE, displays an informational message about the
#'   total number of emails. Default is TRUE (no messages).
#' @param ... Additional arguments passed to [outbound_messages_fetch()].
#'
#' @return A data frame or tibble (if tibble is installed) containing all
#'   retrieved outbound messages.
#'
#'   The function works by first getting an overview of outbound emails to
#'   determine the total count. It then divides this into appropriate batches
#'   for pagination and makes multiple API calls to retrieve all messages.
#'   Results are combined into a single data frame or tibble.
#'
#' @examples
#' \dontrun{
#' # Get all outbound messages with default settings
#' messages <- outbound_messages_collect()
#'
#' # Get messages with a specific token and display count information
#' messages <- outbound_messages_collect(token = "your_api_token", quiet = FALSE)
#' }
#'
#' @export
outbound_messages_collect <- function(token = NULL, quiet = TRUE, ...) {

  stats <- get_outbound_overwiew(token)
  sent <- stats[["Sent"]]

  if (is.null(sent)) {
    rlang::abort(
      "Cannot get number of emails sent.",
      class = "wrong_email_number"
    )
  }

  if (!quiet) {
    rlang::inform(sprintf("Total number of messages is %s.", sent))
  }

  batches <- generate_offset_batches(as.integer(sent))

  out <- Map(
    function(count_val, offset_val) {
      outbound_messages_fetch(
        count = count_val,
        offset = offset_val,
        token = token,
        ...
      )
    },
    batches[["count"]],
    batches[["offset"]]
  )

  if (!length(out)) {
    rlang::abort(
      "Cannot retrieve messages.",
      class = "wrong_api_call"
    )
  }

  msg <-
    lapply(out, function(x) Filter(is.data.frame, x)) |>
    lapply(`[[`, "Messages")

  dat <- Reduce(rbind, msg)

  if (rlang::is_installed("tibble")) {
    return(tibble::as_tibble(dat))
  }

  dat

}
