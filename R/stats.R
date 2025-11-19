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
#' The `Stats` class provides a structured way to pass query parameters
#' to Postmark statistics API endpoints. All properties are optional.
#' The message stream is automatically determined from the client object
#' and does not need to be specified in the params.
#'
#' @examples
#' \dontrun{
#' # Create Stats parameters with all fields
#' stats- Stats(
#'   tag = "welcome-email",
#'   fromdate = "2024-01-01",
#'   todate = "2024-01-31"
#' )
#'
#' # Create Stats parameters with only date range
#' stats <- Stats(
#'   fromdate = "2024-01-01",
#'   todate = "2024-01-31"
#' )
#'
#' # Create empty Stats parameters
#' stats <- Stats()
#' }
#'
#' @seealso
#' \url{https://postmarkapp.com/developer/api/stats-api} for Postmark Stats API
#' documentation
#'
#' @export
Stats <- new_class(
  name = "Stats",
  properties = list(
    tag = new_property(
      class = class_character,
      validator = function(value) {
        if (length(value) && (!is_scalar_character(value) || !nzchar(value))) {
          pstmrk_abort_invalid_scalar_character("tag")
        }
      }
    ),
    fromdate = new_property(
      class = class_character,
      validator = function(value) {
        if (length(value) && (!is_scalar_character(value) || !nzchar(value))) {
          pstmrk_abort_invalid_scalar_character("fromdate")
        }
        date_pattern <- "^\\d{4}-\\d{2}-\\d{2}$"
        if (length(value) && !grepl(date_pattern, value)) {
          pstmrk_abort_invalid_date_format("fromdate", example = "2024-01-01")
        }
      }
    ),
    todate = new_property(
      class = class_character,
      validator = function(value) {
        if (length(value) && (!is_scalar_character(value) || !nzchar(value))) {
          pstmrk_abort_invalid_scalar_character("todate")
        }
        date_pattern <- "^\\d{4}-\\d{2}-\\d{2}$"
        if (length(value) && !grepl(date_pattern, value)) {
          pstmrk_abort_invalid_date_format("todate", example = "2024-12-31")
        }
      }
    )
  ),
  validator = function(self) {
    if (length(self@fromdate) && length(self@todate)) {
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

#' Stats Constructor
#'
#' @description
#' Creates a Stats object for querying statistics from the Postmark API. This is a
#' user-friendly wrapper around the `Stats` S7 class with sensible defaults.
#'
#' @inheritParams Stats
#'
#' @seealso [Stats()] for the S7 class documentation
#'
#' @export
stats <- function(
  tag = character(),
  fromdate = character(),
  todate = character()
) {
  Stats(
    tag = tag,
    fromdate = fromdate,
    todate = todate
  )
}

#' Validate Stats Overview Response
#'
#' @description
#' Validates the response from `/stats/{stream}` endpoint.
#' @noRd
#' @keywords internal
StatsOverviewResponse <- new_class(
  name = "StatsOverviewResponse",
  properties = list(
    Sent = class_integer,
    Bounced = class_integer,
    SMTPApiErrors = class_integer,
    BounceRate = class_numeric,
    SpamComplaints = class_integer,
    SpamComplaintsRate = class_numeric,
    Opens = class_integer,
    UniqueOpens = class_integer,
    Tracked = class_integer,
    WithLinkTracking = class_integer,
    WithOpenTracking = class_integer,
    TotalTrackedLinksSent = class_integer,
    UniqueLinksClicked = class_integer,
    TotalClicks = class_integer,
    WithClientRecorded = class_integer,
    WithPlatformRecorded = class_integer,
    WithReadTimeRecorded = class_integer
  )
)

#' Validate Stats Time Series Response
#'
#' @description
#' Validates responses that contain a `Days` array with time series data.
#' Used for sends, bounces, spam, tracked, opens, and clicks endpoints.
#'
#' @noRd
#' @keywords internal
StatsTsResponse <- new_class(
  name = "StatsTsResponse",
  properties = list(
    Days = new_property(
      class = class_any,
      validator = function(value) {
        # Days should be a data.frame when using simplifyVector = TRUE
        if (length(value) && !is.data.frame(value)) {
          pstmrk_abort(
            "`Days` must be a data.frame",
            class = "postmarkr_error_invalid_stats_response"
          )
        }

        if (is.data.frame(value) && nrow(value) > 0) {
          if (!"Date" %in% names(value)) {
            pstmrk_abort(
              "`Days` data.frame must have a `Date` column",
              class = "postmarkr_error_invalid_stats_response"
            )
          }
        }
      }
    )
    # Additional properties are dynamic based on endpoint
  )
)

#' Convert API Response to Stats Class
#'
#' @description
#' Attempts to convert a raw API response to the appropriate Stats class.
#' If the structure doesn't match expected format, provides an informative error.
#'
#' @param data The raw response data from the API
#' @param endpoint The endpoint path that was called
#'
#' @return A validated Stats response object
#' @noRd
#' @keywords internal
validate_stats_response <- function(data, endpoint) {
  has_timeseries <- function(x) {
    if (grepl("/stats/[^/]+$", x)) "no" else "yes"
  }

  has_timeseries <- has_timeseries(endpoint)

  dat <- switch(
    has_timeseries,
    no = {
      required_fields <- c(
        "Sent",
        "Bounced",
        "SMTPApiErrors",
        "BounceRate",
        "SpamComplaints",
        "SpamComplaintsRate",
        "Opens",
        "UniqueOpens",
        "Tracked",
        "WithLinkTracking",
        "WithOpenTracking",
        "TotalTrackedLinksSent",
        "UniqueLinksClicked",
        "TotalClicks",
        "WithClientRecorded",
        "WithPlatformRecorded",
        "WithReadTimeRecorded"
      )

      missing_fields <- setdiff(required_fields, names(data))
      if (length(missing_fields)) {
        pstmrk_abort_api_change(
          context = "Unexpected structure in Stats overview response.",
          additional_info = paste(
            "Missing required fields:",
            paste(missing_fields, collapse = ", ")
          )
        )
      }

      dat <- try(exec(StatsOverviewResponse, !!!data))

      if (inherits(dat, "try-error")) {
        pstmrk_abort_api_change(
          context = "Unexpected structure in Stats overview response."
        )
      }
      dat
    },
    yes = {
      if (!"Days" %in% names(data)) {
        pstmrk_abort_api_change(
          context = "Expected `Days` field in Stats response but it was not found."
        )
      }
      # For time series responses, we keep the raw data structure
      # but validate the Days array exists and has the right structure
      dat <- try(StatsTsResponse(Days = data$Days))

      if (inherits(dat, "try-error")) {
        pstmrk_abort_api_change(
          context = "Unexpected structure in Stats time series response."
        )
      }
      dat
    }
  )

  dat
}

#' Get Statistics from Postmark
#'
#' @description
#' Generic function to retrieve statistics from various Postmark API endpoints.
#' This provides a flexible way to query different types of statistics using
#' the same interface.
#'
#' @param client A Postmarkr client object created with [Postmarkr()].
#' @param params Optional Stats parameters object created with [Stats()].
#'   If not provided, no query parameters will be sent.
#' @param endpoint A character argument specifying the Stats endpoint path
#'   (e.g., "overview", "sends", "bounces", "opens/emailclients", "clicks",
#'   "clicks/browserfamilies"). The "/stats/" prefix and message stream
#'   will be added automatically.
#' @param ... Additional arguments passed to methods.
#'
#' @return A Response object containing the statistics data.
#'
#' @examples
#' \dontrun{
#' # Create a client
#' client <- Postmarkr(
#'   token = "your-server-token",
#'   message_stream = "outbound"
#' )
#'
#' # Get sent counts with date range
#' params <- Stats(
#'   fromdate = "2024-01-01",
#'   todate = "2024-01-31"
#' )
#' stats_get(client, params)
#'
#' # Get open counts filtered by tag
#' params <- Stats(
#'   tag = "welcome-email",
#'   fromdate = "2024-01-01",
#'   todate = "2024-01-31"
#' )
#' stats_get(client, params, "opens")
#'
#' # Get click statistics by browser family
#' stats_get(client, params, "clicks/browserfamilies")
#' }
#'
#' @seealso
#' \url{https://postmarkapp.com/developer/api/stats-api} for Postmark Stats API
#' documentation
#'
#' @export
stats_get <- new_generic(
  "stats_get",
  c("client", "params"),
  function(client, params, ..., endpoint = NULL) {
    S7_dispatch()
  }
)

method(stats_get, list(Postmarkr, Stats)) <- function(
  client,
  params,
  ...,
  endpoint = NULL
) {
  if (!is.null(endpoint)) {
    if (!is_scalar_character(endpoint) || !nzchar(endpoint)) {
      pstmrk_abort(
        "`endpoint` must be a single non-empty character string",
        class = "postmarkr_error_invalid_endpoint"
      )
    }
  }

  full_endpoint <- paste0(
    "/stats/",
    client@message_stream,
    if (is.null(endpoint)) "" else paste0("/", endpoint)
  )

  query_params <- list()

  if (!S7_inherits(params, Stats)) {
    pstmrk_abort(
      "`params` must be a Stats object",
      class = "postmarkr_error_invalid_params"
    )
  }

  if (length(params@tag)) {
    query_params$tag <- params@tag
  }

  if (length(params@fromdate)) {
    query_params$fromdate <- params@fromdate
  }

  if (length(params@todate)) {
    query_params$todate <- params@todate
  }

  req <- build_req_S7(
    client = client,
    endpoint = full_endpoint,
    method = "GET",
    !!!query_params
  )

  resp <- req_perform(req)

  raw_data <- resp_body_json(resp, simplifyVector = TRUE)

  validated_data <- validate_stats_response(raw_data, full_endpoint)

  Response(
    data = validated_data,
    status = resp_status(resp),
    request = req,
    response = resp,
    success = isFALSE(resp_is_error(resp))
  )
}
