#' Get Overview of Outbound Messages
#'
#' Retrieves a summary of outbound message statistics from the Postmark API.
#' This function fetches aggregated metrics about messages sent through your
#' Postmark account.
#'
#' @inheritParams outbound_messages_fetch
#' @param ... Additional arguments passed to the underlying request functions.
#'
#' @return A list containing outbound message statistics as returned by the
#'   Postmark API.
#'
#' @rdname stats
#'
#' @examples
#' \dontrun{
#' # Using default token from environment
#' stats <- get_outbound_overview()
#'
#' # Using a specific token
#' stats <- get_outbound_overview("your-postmark-token")
#' }
#'
#' @export
stats_outbound_overview <- function(token = NULL, ...) {
  if (is.null(token)) {
    token <- get_token()
  }

  req <- build_req("/stats/outbound", "GET", token)
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}
