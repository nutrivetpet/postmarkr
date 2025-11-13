#' Postmark API Client
#'
#' @description
#' An S7 class representing a Postmark API client for sending emails.
#' This class encapsulates the configuration needed to interact with the
#' Postmark email service.
#'
#' @param token character. Your Postmark Server API Token in UUID format
#'   (e.g., '6777be1f-2a8f-4419-a8b4-fe6ff4490za0'). This token authenticates
#'   your requests to the Postmark API.
#' @param message_stream character. The message stream to use for sending emails.
#'   Must be either "broadcast" (for newsletters and marketing emails) or
#'   "outbound" (for transactional one-to-one triggered emails).
#' @param base_url character. The base URL for the Postmark API. Defaults to
#'   the standard Postmark API endpoint (`https://api.postmarkapp.com`).
#'   **You should not need to change this.** Only modify for testing/mocking
#'   purposes (e.g., pointing to a local mock server during development).
#' @param timeout numeric. Request timeout in seconds for API calls.
#'
#' @details
#' The `Postmarkr` class provides a structured way to configure and use the
#' Postmark API. Before creating an instance, ensure you have:
#' \itemize{
#'   \item A valid Postmark Server API Token
#'   \item Verified sender signatures or domains in your Postmark account
#' }
#'
#' The `token` property is validated to ensure it matches the UUID format
#' required by Postmark. The `message_stream` property is validated to be
#' either "broadcast" or "outbound".
#'
#' @examples
#' \dontrun{
#' # Create a Postmarkr client for transactional emails
#' client <- Postmarkr(
#'   token = "your-server-token-here",
#'   message_stream = "outbound",
#'   timeout = 30
#' )
#'
#' # Create a postmark client for broadcast emails
#' broadcast_client <- Postmarkr(
#'   token = "your-server-token-here",
#'   message_stream = "broadcast"
#' )
#' }
#'
#' @seealso
#' \url{https://postmarkapp.com/developer/api/overview} for Postmark API
#' documentation
#'
#' @export
Postmarkr <- new_class(
  name = "Postmarkr",
  properties = list(
    token = new_property(
      class = class_character,
      validator = function(value) {
        uuid_pattern <- "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
        if (
          !length(value) > 0 || !grepl(uuid_pattern, value, ignore.case = TRUE)
        ) {
          pstmrk_abort(
            "`token` must be a valid UUID format (e.g., '6777be1f-2a8f-4419-a8b4-fe6ff4490za0')",
            class = "postmarkr_error_invalid_token"
          )
        }
      }
    ),
    message_stream = new_property(
      class = class_character,
      validator = function(value) {
        if (!length(value) > 0 || !value %in% c("broadcast", "outbound")) {
          pstmrk_abort(
            "`message_stream` must be either 'broadcast' or 'outbound'",
            class = "postmarkr_error_invalid_message_stream"
          )
        }
      }
    ),
    base_url = new_property(
      class = class_character,
      default = POSTMARK_BASE_URL,
      validator = function(value) {
        if (!length(value) > 0 || !grepl("^https?://", value)) {
          pstmrk_abort(
            "`base_url` must start with http:// or https://",
            class = "postmarkr_error_invalid_base_url"
          )
        }
      }
    ),
    timeout = new_property(
      class = class_numeric,
      default = 1, # TODO: is this a good default?
      validator = function(value) {
        if (!is_integerish(value, n = 1L)) {
          pstmrk_abort(
            "`timeout` must be an integerish",
            class = "postmarkr_error_invalid_timeout"
          )
        }
        if (value < 1) {
          pstmrk_abort(
            "`timeout` must be at least 1 second",
            class = "postmarkr_error_invalid_timeout"
          )
        }
      }
    )
  )
)
