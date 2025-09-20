#' Get Overview of Outbound Messages
#'
#' Retrieves a summary of outbound message statistics from the Postmark API.
#' This function fetches aggregated metrics about messages sent through your
#' Postmark account.
#'
#' @param ... Additional arguments passed to the underlying request functions.
#'
#' @return A list containing outbound message statistics as returned by the
#'   Postmark API.
#'
#' @rdname stats
#'
#' @examples
#' \dontrun{
#' # Get outbound message statistics
#' stats <- stats_outbound_overview()
#' }
#'
#' @export
stats_outbound_overview <- function(...) {
  req <- build_req("/stats/outbound", "GET", ...)
  resp <- req_perform(req)
  resp_body_json(resp, simplifyVector = FALSE)
}
