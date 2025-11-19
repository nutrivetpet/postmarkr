#' @include postmarkr.R as_api_body.R
NULL

#' Template Object
#'
#' @description
#' Creates a Template object for sending template-based emails via the Postmark
#' API. This provides a structured way to send emails using predefined Postmark
#' templates. It supports:
#' \itemize{
#'   \item Template variable substitution via `template_model`
#'   \item Multiple recipients via To, Cc, and Bcc fields (max 50 combined)
#'   \item Inline CSS processing for better email client compatibility
#'   \item Open and click tracking configuration
#'   \item Email categorization via tags
#'   \item File attachments and custom headers
#'   \item Metadata for internal tracking
#' }
#'
#' @inheritSection email Limitations
#'
#' @inheritParams email
#' @param id Integer scalar. The template ID in Postmark. Must be a positive
#'   integer corresponding to an existing template in your account. Typically
#'   an 8-digit number (e.g., 12345678). **Mutually exclusive with `alias`**.
#' @param alias Character scalar. The template alias in Postmark. A named
#'   identifier for your template (e.g., "welcome-email", "password-reset").
#'   **Mutually exclusive with `id`**.
#' @param template_model List. **Required.** Named list of variables to populate
#'   in the template. Keys must match template variable names. Can include nested
#'   lists for complex data structures. Example:
#'   `list(user_name = "John", company = list(name = "ACME"))`.
#' @param inline_css Logical scalar. Whether to process CSS in `<style>` tags
#'   into inline style attributes. Improves compatibility with email clients.
#'   Default is TRUE for better rendering.
#'
#' @examples
#' \dontrun{
#' # Simple template email using template ID
#' simple_template <- template(
#'   from = "sender@example.com",
#'   to = "recipient@example.com",
#'   id = 12345678L,
#'   template_model = list(name = "John", message = "Welcome!")
#' )
#'
#' # Template email using template alias
#' alias_template <- template(
#'   from = "sender@example.com",
#'   to = "recipient@example.com",
#'   alias = "welcome-email",
#'   template_model = list(name = "John", message = "Welcome!")
#' )
#'
#' # Template with tracking, attachments, and metadata
#' full_template <- template(
#'   from = "notifications@example.com",
#'   to = c("user1@example.com", "user2@example.com"),
#'   cc = "manager@example.com",
#'   id = 67890123L,
#'   template_model = list(
#'     order_id = "ORD-789",
#'     status = "Completed"
#'   ),
#'   inline_css = TRUE,
#'   tag = "order-confirmation",
#'   reply_to = "support@example.com",
#'   headers = list(
#'     list(Name = "X-Priority", Value = "High")
#'   ),
#'   track_opens = TRUE,
#'   track_links = "HtmlOnly",
#'   attachments = list(
#'     list(
#'       Name = "invoice.pdf",
#'       Content = "base64content",
#'       ContentType = "application/pdf"
#'     )
#'   ),
#'   metadata = list(
#'     customer_id = "12345",
#'     order_type = "subscription"
#'   )
#' )
#' }
#'
#' @seealso
#' \url{https://postmarkapp.com/developer/api/templates-api#send-email-with-template}
#' for complete Postmark template API documentation
#'
#' @rdname template
#' @export
template <- function(
  from = character(0),
  to = character(0),
  template_model = list(),
  id = numeric(0),
  alias = character(0),
  cc = character(0),
  bcc = character(0),
  inline_css = logical(0),
  tag = character(0),
  reply_to = character(0),
  headers = list(),
  track_opens = logical(0),
  track_links = character(0),
  attachments = list(),
  metadata = list()
) {
  Template(
    from = from,
    to = to,
    id = id,
    alias = alias,
    template_model = template_model,
    cc = cc,
    bcc = bcc,
    inline_css = inline_css,
    tag = tag,
    reply_to = reply_to,
    headers = headers,
    track_opens = track_opens,
    track_links = track_links,
    attachments = attachments,
    metadata = metadata
  )
}

#' @rdname template
#' @export
Template <- new_class(
  "Template",
  properties = list(
    from = new_property(
      class = class_character,
      validator = function(value) {
        if (!length(value)) {
          pstmrk_abort(
            "`from` is required",
            class = "postmarkr_error_template_missing_from"
          )
        }
        validate_email(value, arg_name = "from")
      }
    ),
    to = new_property(
      class = class_character,
      validator = function(value) {
        if (!length(value)) {
          pstmrk_abort(
            "`to` is required",
            class = "postmarkr_error_template_missing_to"
          )
        }
        validate_email(value, arg_name = "to")
      }
    ),
    id = new_property(
      class = class_numeric,
      validator = function(value) {
        if (length(value) && (!is_scalar_integerish(value) || value <= 0)) {
          pstmrk_abort_template_invalid_id()
        }
      }
    ),
    alias = new_property(
      class = class_character,
      validator = function(value) {
        if (length(value) && (!is_scalar_character(value) || !nzchar(value))) {
          pstmrk_abort_invalid_scalar_character("alias")
        }
      }
    ),
    template_model = new_property(
      class = class_list,
      validator = function(value) {
        if (!length(value)) {
          pstmrk_abort(
            "`template_model` is required",
            class = "postmarkr_error_template_missing_model"
          )
        }
        if (!is_named(value)) {
          pstmrk_abort(
            "`template_model` must be a named list",
            class = "postmarkr_error_invalid_template_model"
          )
        }
      }
    ),
    cc = new_property(
      class = class_character,
      validator = function(value) {
        if (length(value)) {
          validate_email(value, arg_name = "cc")
        }
      }
    ),
    bcc = new_property(
      class = class_character,
      validator = function(value) {
        if (length(value)) {
          validate_email(value, arg_name = "bcc")
        }
      }
    ),
    inline_css = class_logical,
    tag = new_property(
      class = class_character,
      validator = function(value) {
        if (length(value)) {
          if (
            !is_scalar_character(value) || !nzchar(value) || nchar(value) > 1000
          ) {
            pstmrk_abort_template_invalid_tag()
          }
        }
      }
    ),
    reply_to = new_property(
      class = class_character,
      validator = function(value) {
        if (length(value)) {
          validate_email(value, arg_name = "reply_to")
        }
      }
    ),
    headers = class_list,
    track_opens = class_logical,
    track_links = new_property(
      class = class_character,
      validator = function(value) {
        if (length(value)) {
          valid_options <- c("None", "HtmlAndText", "HtmlOnly", "TextOnly")
          if (!value %in% valid_options) {
            pstmrk_abort_email_invalid_track_links(value)
          }
        }
      }
    ),
    attachments = class_list,
    metadata = class_list
  ),
  validator = function(self) {
    has_id <- length(self@id) > 0
    has_alias <- length(self@alias) > 0

    if (!has_id && !has_alias) {
      pstmrk_abort(
        "Either `id` or `alias` must be provided",
        class = "postmarkr_error_template_missing_identifier"
      )
    }

    if (has_id && has_alias) {
      pstmrk_abort(
        "Cannot provide both `id` and `alias`. Use one or the other.",
        class = "postmarkr_error_template_conflicting_identifiers"
      )
    }

    total_recipients <- length(self@to) + length(self@cc) + length(self@bcc)
    if (total_recipients > POSTMARK_MAX_RECIPIENTS_SINGLE) {
      pstmrk_abort_template_too_many_recipients(total_recipients)
    }
  }
)
