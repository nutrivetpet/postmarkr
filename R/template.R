#' @include postmarkr.R as_api_body.R

#' Template - Email with template
#'
#' @description
#' An S7 class representing a template-based email message to be sent via the
#' Postmark API. This class encapsulates all the properties needed to send
#' an email using a Postmark template.
#'
#' @details
#' The `Template` class provides a structured way to send emails using
#' predefined Postmark templates. It supports:
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
#' **Important Limitations:**
#' \itemize{
#'   \item Maximum 50 total recipients (To, Cc, Bcc combined)
#'   \item Template must exist in your Postmark account
#'   \item Template ID must be a positive integer (typically 8 digits)
#'   \item One tag per message, maximum 1000 characters
#'   \item Maximum 10 MB total message size (including attachments)
#' }
#'
#' @param from Character scalar. **Required.** Sender email address (must be
#'   verified in Postmark). Supports name formatting: `"John Doe <email@@example.com>"`
#' @param to Character vector. **Required.** Recipient email addresses. Multiple
#'   recipients allowed (max 50 total across To, Cc, Bcc)
#' @param id Integer scalar. The template ID in Postmark. Must be a positive
#'   integer corresponding to an existing template in your account. Typically
#'   an 8-digit number (e.g., 12345678). **Either `id` or `alias` must be
#'   provided, but not both.**
#' @param alias Character scalar. The template alias in Postmark. A named
#'   identifier for your template (e.g., "welcome-email", "password-reset").
#'   **Either `id` or `alias` must be provided, but not both.**
#' @param template_model List. **Required.** Named list of variables to populate
#'   in the template. Keys must match template variable names. Can include nested
#'   lists for complex data structures. Example:
#'   `list(user_name = "John", company = list(name = "ACME"))`.
#' @param cc Character vector. Carbon copy recipients. Visible to all recipients.
#' @param bcc Character vector. Blind carbon copy recipients. Hidden from
#'   other recipients.
#' @param inline_css Logical scalar. Whether to process CSS in `<style>` tags
#'   into inline style attributes. Improves compatibility with email clients.
#'   Default is TRUE for better rendering.
#' @param tag Character scalar. Category tag for statistics and filtering.
#'   One tag per message, maximum 1000 characters. Examples: "welcome-email",
#'   "password-reset", "invoice".
#' @param reply_to Character scalar. Reply-To address for responses.
#' @param headers List of lists. Custom email headers. Each element should
#'   have `Name` and `Value` keys. Example: `list(list(Name = "X-Priority",
#'   Value = "High"))`.
#' @param track_opens Logical scalar. Enable open tracking using invisible
#'   pixel. Provides insights on when and where emails are opened.
#' @param track_links Character scalar. Link tracking mode. Options:
#'   `"None"` (no tracking), `"HtmlAndText"` (track all links),
#'   `"HtmlOnly"` (HTML links only), `"TextOnly"` (text links only).
#' @param attachments List of lists. File attachments. Each attachment should
#'   have `Name`, `Content` (base64-encoded), and `ContentType` keys.
#'   Total message size including attachments limited to 10 MB.
#' @param metadata List. Key-value pairs for internal tracking. Does not
#'   affect email delivery. Example: `list(customer_id = "12345",
#'   campaign = "onboarding")`.
#'
#' @examples
#' \dontrun{
#' # Simple template email using template ID
#' simple_template <- Template(
#'   from = "sender@example.com",
#'   to = "recipient@example.com",
#'   id = 12345678L,
#'   template_model = list(name = "John", message = "Welcome!")
#' )
#'
#' # Template email using template alias
#' alias_template <- Template(
#'   from = "sender@example.com",
#'   to = "recipient@example.com",
#'   alias = "welcome-email",
#'   template_model = list(name = "John", message = "Welcome!")
#' )
#'
#' # Template with nested model and inline CSS
#' complex_template <- Template(
#'   from = "sender@example.com",
#'   to = "recipient@example.com",
#'   id = 12345678L,
#'   template_model = list(
#'     user_name = "Jane Smith",
#'     company = list(name = "ACME Corp")
#'   ),
#'   inline_css = TRUE,
#'   track_opens = TRUE,
#'   tag = "onboarding"
#' )
#'
#' # Template with tracking, attachments, and metadata
#' full_template <- Template(
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
#' @export
Template <- new_class(
  "Template",
  properties = list(
    # Required properties
    from = new_property(
      class = class_character,
      validator = function(value) {
        if (!length(value)) {
          pstmrk_abort(
            "`from` is required",
            class = "postmarkr_error_template_missing_from"
          )
        }
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
      }
    ),
    id = new_property(
      class = class_integer,
      validator = function(value) {
        if (length(value) && (!is_scalar_integer(value) || value <= 0)) {
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
    # Optional properties
    cc = class_character,
    bcc = class_character,
    inline_css = class_logical,
    tag = new_property(
      class = class_character,
      validator = function(value) {
        if (length(value)) {
          if (!is_scalar_character(value) || !nzchar(value) || nchar(value) > 1000) {
            pstmrk_abort_template_invalid_tag()
          }
        }
      }
    ),
    reply_to = class_character,
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
    # Validate that either id OR alias is provided (mutually exclusive)
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

    # Validate recipient count
    total_recipients <- length(self@to) + length(self@cc) + length(self@bcc)
    if (total_recipients > POSTMARK_MAX_RECIPIENTS_SINGLE) {
      pstmrk_abort_template_too_many_recipients(total_recipients)
    }
  }
)

#' Send an Email Using a Template
#'
#' Sends an email using a predefined template.
#'
#' @inheritParams email_send_single
#' @param id A single integer. The template ID in Postmark.
#' @param template_model A named list. Variables to be populated in the
#'   template.
#' @param tag A single character string. Optional tag for categorizing the
#'   email. Maximum 1000 characters. Default is NULL.
#' @param track_opens A logical value. Whether to track when recipients open the
#'   email. Default is FALSE.
#'
#' @return A data frame or tibble (if tibble is installed) containing the
#'   response details, invisibly.
#'
#' @examples
#' \dontrun{
#' template_send_email(
#'   from = "sender@example.com",
#'   to = c("recipient1@example.com", "recipient2@example.com"),
#'   id = 12345,
#'   template_model = list(
#'     name = "John",
#'     message = "Hello from Postmark!"
#'   ),
#'   msg_stream = "outbound",
#'   tag = "welcome-email",
#'   track_opens = TRUE
#' )
#' }
#'
#' @export
template_send_email <- function(
  from,
  to,
  id,
  template_model,
  msg_stream,
  tag = NULL,
  track_opens = FALSE
) {
  template_send_email_impl(
    from = from,
    to = to,
    id = id,
    template_model = template_model,
    msg_stream = msg_stream,
    tag = tag,
    track_opens = track_opens,
    env = "live"
  )
}

template_send_email_impl <- function(
  from,
  to,
  id,
  template_model,
  msg_stream,
  tag = NULL,
  track_opens = FALSE,
  env = c("live", "test")
) {
  recipients_error <- sprintf(
    "`to` must have %d or fewer recipients",
    POSTMARK_MAX_RECIPIENTS_SINGLE
  )
  stopifnot(
    "`from` must be a single character string" = is_scalar_character(from),
    "`to` must be a character vector" = is_character(to),
    recipients_error = length(to) <= POSTMARK_MAX_RECIPIENTS_SINGLE,
    "`id` must be a single integer" = is_scalar_integer(id),
    "`template_model` must be a list" = is_list(template_model),
    "`template_model` must be a named list" = is_named(template_model),
    "`tag` must be 1000 characters or fewer" = nchar(tag) <= 1e3L,
    "`track_opens` must be a single logical value" = is_scalar_logical(
      track_opens
    )
  )

  if (!is.null(tag)) {
    stopifnot(
      "`tag` must be a single character string" = is_scalar_character(tag),
      "`tag` must be 1000 characters or fewer" = nchar(tag) <= 1e3L
    )
  }

  msg_stream <- arg_match(msg_stream, c("outbound", "broadcast"))

  to <- collapse_comma(to)

  bdy <- list(
    From = from,
    To = to,
    Tag = tag,
    TemplateId = id,
    TemplateModel = as.list(template_model),
    MessageStream = msg_stream,
    TrackOpens = track_opens
  )

  req <-
    build_req("/email/withTemplate/", "POST", env) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(bdy)

  resp <- req_perform(req)

  if (resp_is_error(resp)) {
    resp_check_status(resp)
  }

  dat <- resp_body_json(resp, simplifyVector = TRUE)

  if (is_installed("tibble")) {
    dat <- tibble::as_tibble(dat)
  }

  invisible(dat)
}

#' Send Batch Email Using a Template
#'
#' Send email to more than 500 recipients, sending multiple POST requests via
#' `req_perform_sequential()`.
#'
#' @inheritParams template_send_email
#'
#' @return A data frame or tibble (if tibble is installed) containing the
#'   response details, invisibly.
#'
#' @export
template_send_email_batch <- function(
  from,
  to,
  id,
  template_model,
  msg_stream,
  tag = NULL,
  track_opens = FALSE
) {
  template_send_email_batch_impl(
    from = from,
    to = to,
    id = id,
    template_model = template_model,
    msg_stream = msg_stream,
    tag = tag,
    track_opens = track_opens,
    env = "live"
  )
}

template_send_email_batch_impl <- function(
  from,
  to,
  id,
  template_model,
  msg_stream,
  tag = NULL,
  track_opens = FALSE,
  env = c("live", "test")
) {
  stopifnot(
    "`from` must be a single character string" = is_scalar_character(from),
    "`to` must be a character vector" = is_character(to),
    "`id` must be a single integer" = is_scalar_integer(id),
    "`template_model` must be a list" = is_list(template_model),
    "`template_model` must be a named list" = is_named(template_model),
    "`track_opens` must be a single logical value" = is_scalar_logical(
      track_opens
    )
  )

  if (!is.null(tag)) {
    stopifnot(
      "`tag` must be a single character string" = is_scalar_character(tag),
      "`tag` must be 1000 characters or fewer" = nchar(tag) <= 1e3L
    )
  }

  msg_stream <- arg_match(msg_stream, c("outbound", "broadcast"))

  template_model <- rep_list(template_model, length(to))

  bdy <- Map(
    function(from, to, id, template_model, track_opens, msg_stream) {
      list(
        From = from,
        To = to,
        TemplateId = id,
        TemplateModel = template_model,
        TrackOpens = track_opens,
        MessageStream = msg_stream
      )
    },
    from,
    to,
    id,
    template_model,
    track_opens,
    msg_stream,
    USE.NAMES = FALSE
  )

  if (!is.null(tag)) {
    bdy <- Map(
      function(x, y) {
        x[["Tag"]] <- y
        x
      },
      bdy,
      tag
    )
  }

  bdy_lst <- unname(split(bdy, (seq_along(bdy) - 1) %/% POSTMARK_MAX_BATCH_SIZE))

  bdy <- lapply(bdy_lst, \(x) list("Messages" = x))

  req_lst <- lapply(
    bdy,
    function(x) {
      build_req("/email/batchWithTemplates/", "POST", env) |>
        req_headers("Content-Type" = "application/json") |>
        req_body_json(x)
    }
  )

  resp <- req_perform_sequential(
    req_lst,
    on_error = "continue",
    progress = TRUE
  )

  dat_lst <- lapply(resp, \(x) resp_body_json(x, simplifyVector = TRUE))
  dat <- dplyr::bind_rows(dat_lst)

  if (is_installed("tibble")) {
    dat <- tibble::as_tibble(dat)
  }

  invisible(dat)
}

#' List Templates
#'
#' Retrieves a list of templates from the Postmark API. Templates
#' can be filtered by type and paginated using count and offset parameters.
#'
#' @param count An integer specifying the number of templates to retrieve.
#' @param type A string specifying the template type to filter by: "all",
#'   "standard", or "layout". Defaults to "all".
#'
#' @return A data frame (or tibble if tibble is installed) containing the
#'   templates information. The returned data includes template details from the
#'   Postmark API.
#'
#' @examples
#' \dontrun{
#' # Get the first 10 templates
#' templates <- template_list(count = 10)
#'
#' # Get only layout templates
#' layouts <- template_list(count = 50, type = "layout")
#'
#' # Get all templates with pagination
#' templates <- template_list(count = 100)
#' }
#' @export
template_list <- function(count, type = "all") {
  template_list_impl(count = count, type = type, env = "live")
}

template_list_impl <- function(count, type = "all", env = c("live", "test")) {
  stopifnot(
    "`count` must be a single integer" = is_scalar_integer(count),
    "`count` must be greater than 0" = count > 0
  )

  typ <- arg_match(type, c("all", "standard", "layout"))

  req <- build_req(
    "templates",
    "GET",
    env,
    count = count,
    offset = 0L,
    type = capitalize_first(type)
  )

  resp <- req_perform(req)
  out <- resp_body_json(resp, simplifyVector = TRUE)
  dat <- out[["Templates"]]

  if (is_installed("tibble")) {
    dat <- tibble::as_tibble(dat)
  }

  dat
}
