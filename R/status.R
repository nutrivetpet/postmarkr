#' Check Postmark Service Status
#'
#' @description
#' Queries the Postmark status API to check if the service is operational.
#' This function does not require authentication and can be used to verify
#' that Postmark's infrastructure is running before attempting to send emails.
#'
#' @param timeout numeric. Request timeout in seconds. Defaults to 10 seconds.
#'
#' @return A named list with a state element that indicates whether the service is "operational", "degraded" or "under_maintenance".
#'
#' @examples
#' \dontrun{
#' # Check if Postmark is operational
#' status <- status_get()
#' print(status)
#'
#' # Check with custom timeout
#' status <- status_get(timeout = 5)
#' }
#'
#' @seealso
#' \url{https://status.postmarkapp.com/api} for the Postmark status page
#'
#' @export
status_get <- function(timeout = 10) {
  if (!is_scalar_integerish(timeout) || timeout < 1) {
    pstmrk_abort(
      "`timeout` must be a positive integer",
      class = "postmarkr_error_invalid_timeout"
    )
  }

  req <-
    request("https://status.postmarkapp.com") |>
    req_url_path_append("/api/v1/status") |>
    req_user_agent("postmarkr (https://nutrivetpet.github.io/postmarkr/)") |>
    req_timeout(timeout)

  resp <- req_perform(req)

  body <- resp_body_json(resp)

  list(
    status = body$page$state %||% NA_character_,
    state_text = body$page$state_text %||% NA_character_,
    updated_at = body$page$updated_at %||% NA_character_
  )
}
