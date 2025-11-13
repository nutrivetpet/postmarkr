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
#'
#' @details
#' The `stats` class provides a structured way to pass query parameters
#' to Postmark statistics API endpoints. All properties are optional.
#' The message stream is automatically determined from the client object
#' and does not need to be specified in the params.
#'
#'
#' @examples
#' \dontrun{
#' # Create stats parameters with all fields
#' params <- stats(
#'   tag = "welcome-email",
#'   fromdate = "2024-01-01",
#'   todate = "2024-01-31"
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

#' Get Statistics from Postmark
#'
#' @description
#' Generic function to retrieve statistics from various Postmark API endpoints.
#' This provides a flexible way to query different types of statistics using
#' the same interface.
#'
#' @param client A postmarkr client object created with [postmark()].
#' @param endpoint character. The stats endpoint path (e.g., "overview",
#'   "sends", "bounces", "opens/emailclients", "clicks",
#'   "clicks/browserfamilies"). The "/stats/" prefix and message stream
#'   will be added automatically.
#' @param params Optional stats parameters object created with [stats()].
#'   If not provided, no query parameters will be sent.
#'
#' @return A postmarkr_response object containing the statistics data.
#'
#' @examples
#' \dontrun{
#' # Create a client
#' client <- postmarkr(
#'   token = "your-server-token",
#'   message_stream = "outbound"
#' )
#'
#' # Get overview statistics
#' stats_get(client, "overview")
#'
#' # Get sent counts with date range
#' params <- stats(
#'   fromdate = "2024-01-01",
#'   todate = "2024-01-31"
#' )
#' stats_get(client, "sends", params)
#'
#' # Get open counts filtered by tag
#' params <- stats(
#'   tag = "welcome-email",
#'   fromdate = "2024-01-01",
#'   todate = "2024-01-31"
#' )
#' stats_get(client, "opens", params)
#'
#' # Get click statistics by browser family
#' stats_get(client, "clicks/browserfamilies", params)
#' }
#'
#' @seealso
#' \url{https://postmarkapp.com/developer/api/stats-api} for Postmark Stats API
#' documentation
#'
#' @export
stats_get <- new_generic("stats_get", c("client", "endpoint"))

method(stats_get, list(postmarkr, class_character)) <- function(
  client,
  endpoint,
  params = NULL
) {
  if (!is_scalar_character(endpoint) || !nzchar(endpoint)) {
    pstmrk_abort(
      "`endpoint` must be a single non-empty character string",
      class = "postmarkr_error_invalid_endpoint"
    )
  }

  full_endpoint <- paste0(
    "/stats/",
    client@message_stream,
    if (endpoint != "") paste0("/", endpoint) else ""
  )

  query_params <- list()

  # Add messagestream from client (convert message_stream to messagestream)
  if (length(client@message_stream) > 0) {
    query_params$messagestream <- client@message_stream
  }

  if (!is.null(params)) {
    if (!inherits(params, "stats")) {
      pstmrk_abort(
        "`params` must be a stats object or NULL",
        class = "postmarkr_error_invalid_params"
      )
    }

    if (length(params@tag) > 0) {
      query_params$tag <- params@tag
    }

    if (length(params@fromdate) > 0) {
      query_params$fromdate <- params@fromdate
    }

    if (length(params@todate) > 0) {
      query_params$todate <- params@todate
    }
  }

  req <- build_req_s7(
    client = client,
    endpoint = full_endpoint,
    method = "GET",
    !!!query_params
  )

  resp <- req_perform(req)

  postmarkr_response(
    data = resp_body_json(resp, simplifyVector = TRUE),
    status = resp_status(resp),
    request = req,
    response = resp,
    success = isFALSE(resp_is_error(resp))
  )
}
