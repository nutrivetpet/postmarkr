#' Stats Query Parameters
#'
#' @description
#' An S7 class representing query parameters for retrieving statistics from
#' the Postmark API. This class encapsulates the common filtering parameters
#' used across various statistics endpoints.
#'
#' @param tag character. Optional tag to filter statistics by. Tags are used to
#'   categorize emails for detailed tracking and reporting.
#' @param fromdate character. Optional start date for the statistics query in
#'   YYYY-MM-DD format (e.g., "2024-01-01").
#' @param todate character. Optional end date for the statistics query in
#'   YYYY-MM-DD format (e.g., "2024-12-31").
#' @param messagestream character. Optional message stream to filter by.
#'   Must be either "outbound" (transactional emails) or "broadcast"
#'   (bulk/marketing emails).
#'
#' @details
#' The `stats` class provides a structured way to pass query parameters
#' to Postmark statistics API endpoints. All properties are optional.
#'
#'
#' @examples
#' \dontrun{
#' # Create stats parameters with all fields
#' params <- stats(
#'   tag = "welcome-email",
#'   fromdate = "2024-01-01",
#'   todate = "2024-01-31",
#'   messagestream = "outbound"
#' )
#'
#' # Create stats parameters with only date range
#' params <- stats(
#'   fromdate = "2024-01-01",
#'   todate = "2024-01-31"
#' )
#'
#' # Create empty stats parameters
#' params <- stats()
#' }
#'
#' @seealso
#' \url{https://postmarkapp.com/developer/api/stats-api} for Postmark Stats API
#' documentation
#'
#' @export
stats <- new_class(
  name = "stats",
  properties = list(
    tag = new_property(
      class = class_character,
      validator = function(value) {
        if (
          length(value) > 0 && (!is_scalar_character(value) || !nzchar(value))
        ) {
          pstmrk_abort(
            "`tag` must be a single non-empty character string if provided",
            class = "postmarkr_error_invalid_tag"
          )
        }
      }
    ),
    fromdate = new_property(
      class = class_character,
      validator = function(value) {
        if (
          length(value) > 0 && (!is_scalar_character(value) || !nzchar(value))
        ) {
          pstmrk_abort(
            "`fromdate` must be a single non-empty character string if provided",
            class = "postmarkr_error_invalid_fromdate"
          )
        }
        date_pattern <- "^\\d{4}-\\d{2}-\\d{2}$"
        if (length(value) > 0 && !grepl(date_pattern, value)) {
          pstmrk_abort(
            "`fromdate` must be in YYYY-MM-DD format (e.g., '2024-01-01') if provided",
            class = "postmarkr_error_invalid_fromdate"
          )
        }
      }
    ),
    todate = new_property(
      class = class_character,
      validator = function(value) {
        if (
          length(value) > 0 && (!is_scalar_character(value) || !nzchar(value))
        ) {
          pstmrk_abort(
            "`todate` must be a single non-empty character string if provided",
            class = "postmarkr_error_invalid_todate"
          )
        }
        date_pattern <- "^\\d{4}-\\d{2}-\\d{2}$"
        if (length(value) > 0 && !grepl(date_pattern, value)) {
          pstmrk_abort(
            "`todate` must be in YYYY-MM-DD format (e.g., '2024-12-31') if provided",
            class = "postmarkr_error_invalid_todate"
          )
        }
      }
    ),
    messagestream = new_property(
      class = class_character,
      validator = function(value) {
        if (
          length(value) > 0 &&
            (!is_scalar_character(value) ||
              !nzchar(value) ||
              !value %in% c("outbound", "broadcast"))
        ) {
          pstmrk_abort(
            "`messagestream` must be 'outbound' or 'broadcast' if provided",
            class = "postmarkr_error_invalid_messagestream"
          )
        }
      }
    )
  ),
  validator = function(self) {
    if (length(self@fromdate) > 0 && length(self@todate) > 0) {
      from <- as.Date(self@fromdate, format = "%Y-%m-%d")
      to <- as.Date(self@todate, format = "%Y-%m-%d")
      if (from > to) {
        pstmrk_abort(
          "`fromdate` must not be after `todate`",
          class = "postmarkr_error_invalid_date_range"
        )
      }
    }
  }
)
