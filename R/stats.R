#' Get Overview of Outbound Messages
#'
#' Retrieves a summary of outbound message statistics from the Postmark API.
#' This function fetches aggregated metrics about messages sent through your
#' Postmark account.
#'
#' @param client A client object containing your API credentials and
#'   configuration. Create one using [postmark()].
#' @param ... Additional arguments passed to the underlying request functions.
#'
#' @return A list containing outbound message statistics as returned by the
#'   Postmark API.
#'
#' @rdname stats
#'
#' @examples
#' \dontrun{
#' # Create a postmark client
#' client <- postmarkr(
#'   token = "your-server-token-here",
#'   message_stream = "transactional"
#' )
#'
#' # Get outbound message statistics
#' stats <- stats_outbound_overview(client)
#' }
#'
#' @export
stats_outbound_overview <- new_generic(
  name = "stats_outbound_overview",
  dispatch_args = "client"
)

#' @export
method(stats_outbound_overview, postmarkr) <- function(client, ...) {
  req <- build_req_s7(client, "/stats/outbound", "GET", ...)
  resp <- req_perform(req)
  resp_body_json(resp, simplifyVector = FALSE)
}
