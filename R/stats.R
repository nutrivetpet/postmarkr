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
#' params <- Stats(
#'   tag = "welcome-email",
#'   fromdate = "2024-01-01",
#'   todate = "2024-01-31"
#' )
#'
#' # Create Stats parameters with only date range
#' params <- Stats(
#'   fromdate = "2024-01-01",
#'   todate = "2024-01-31"
#' )
#'
#' # Create empty Stats parameters
#' params <- Stats()
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
#' @param client A Postmarkr client object created with [postmark()].
#' @param endpoint character. The Stats endpoint path (e.g., "overview",
#'   "sends", "bounces", "opens/emailclients", "clicks",
#'   "clicks/browserfamilies"). The "/stats/" prefix and message stream
#'   will be added automatically.
#' @param params Optional Stats parameters object created with [Stats()].
#'   If not provided, no query parameters will be sent.
#'
#' @return A postmarkr_response object containing the statistics data.
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
stats_get <- new_generic("stats_get", c("client", "params"))

#' Validate Stats Overview Response
#'
#' @description
#' Validates the response from `/stats/{stream}` endpoint.
#' @noRd
#' @keywords internal
stats_overview_response <- new_class(
  name = "stats_overview_response",
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
stats_timeseries_response <- new_class(
  name = "stats_timeseries_response",
  properties = list(
    Days = new_property(
      class = class_any,
      validator = function(value) {
        # Days should be a data.frame when using simplifyVector = TRUE
        if (length(value) > 0 && !is.data.frame(value)) {
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
      if (length(missing_fields) > 0) {
        pstmrk_abort(
          c(
            "Unexpected structure in Stats overview response.",
            "i" = "This might indicate a change in the Postmark API.",
            "i" = "Please report this issue at: https://github.com/nutrivetpet/postmarkr/issues",
            "x" = paste(
              "Missing required fields:",
              paste(missing_fields, collapse = ", ")
            )
          ),
          class = "postmarkr_error_stats_response_validation"
        )
      }

      dat <- try(exec(stats_overview_response, !!!data))

      if (inherits(dat, "try-error")) {
        pstmrk_abort(
          c(
            "Unexpected structure in Stats overview response.",
            "i" = "This might indicate a change in the Postmark API.",
            "i" = "Please report this issue at: https://github.com/nutrivetpet/postmarkr/issues"
          ),
          class = "postmarkr_error_stats_response_validation"
        )
      }
      dat
    },
    yes = {
      if (!"Days" %in% names(data)) {
        pstmrk_abort(
          c(
            "Expected `Days` field in Stats response but it was not found.",
            "i" = "This might indicate a change in the Postmark API.",
            "i" = "Please report this issue at: https://github.com/nutrivetpet/postmarkr/issues"
          ),
          class = "postmarkr_error_stats_response_validation"
        )
      }
      # For time series responses, we keep the raw data structure
      # but validate the Days array exists and has the right structure
      dat <- try(stats_timeseries_response(Days = data$Days))

      if (inherits(dat, "try-error")) {
        pstmrk_abort(
          c(
            "Unexpected structure in Stats time series response.",
            "i" = "This might indicate a change in the Postmark API.",
            "i" = "Please report this issue at: https://github.com/nutrivetpet/postmarkr/issues"
          ),
          class = "postmarkr_error_stats_response_validation"
        )
      }
      dat
    }
  )

  dat
}

method(stats_get, list(Postmarkr, Stats)) <- function(
  client,
  params,
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
      "`params` must be a Stats object or NULL",
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

  req <- build_req_s7(
    client = client,
    endpoint = full_endpoint,
    method = "GET",
    !!!query_params
  )

  resp <- req_perform(req)

  raw_data <- resp_body_json(resp, simplifyVector = TRUE)

  validated_data <- validate_stats_response(raw_data, full_endpoint)

  postmarkr_response(
    data = validated_data,
    status = resp_status(resp),
    request = req,
    response = resp,
    success = isFALSE(resp_is_error(resp))
  )
}
