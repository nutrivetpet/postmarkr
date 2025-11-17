#' Email - Single email message
#'
#' @description
#' An S7 class representing a single email message to be sent via the Postmark
#' API. This class encapsulates all the properties needed to compose and send
#' an email, including content, recipients, tracking settings, and attachments.
#'
#' @details
#' The `Email` class provides a structured way to compose emails for sending
#' through Postmark. It supports:
#' \itemize{
#'   \item HTML and plain text content (mutually exclusive)
#'   \item Multiple recipients via To, Cc, and Bcc fields (max 50 combined)
#'   \item Custom email headers for routing and priority
#'   \item Metadata for internal tracking
#'   \item File attachments and inline images
#'   \item Open and click tracking configuration
#'   \item Email categorization via tags
#' }
#'
#' **Important Limitations:**
#' \itemize{
#'   \item Maximum 50 total recipients (To, Cc, Bcc combined)
#'   \item Maximum 10 MB total message size (including attachments)
#'   \item Cannot provide both `html_body` and `text_body` simultaneously
#'   \item Must provide either `html_body` or `text_body`
#'   \item One tag per message, maximum 1000 characters
#' }
#'
#' @param from Character scalar. Sender email address (must be verified in
#'   Postmark). Supports name formatting: `"John Doe <email@@example.com>"`
#' @param to Character vector. Recipient email addresses. Multiple recipients
#'   allowed (max 50 total across To, Cc, Bcc)
#' @param cc Character vector. Carbon copy recipients. Visible to all recipients.
#' @param bcc Character vector. Blind carbon copy recipients. Hidden from
#'   other recipients.
#' @param subject Character scalar. Email subject line.
#' @param tag Character scalar. Category tag for statistics and filtering.
#'   One tag per message, maximum 1000 characters. Examples: "welcome-email",
#'   "password-reset", "invoice".
#' @param html_body Character scalar. HTML content of the email. Use for
#'   rich formatting, images, and styling. Mutually exclusive with
#'   `text_body`. Maximum 5 MB.
#' @param text_body Character scalar. Plain text content of the email.
#'   Use for simple text-only emails. Mutually exclusive with `html_body`.
#'   Maximum 5 MB.
#' @param reply_to Character scalar. Reply-To address for responses.
#' @param metadata List. Key-value pairs for internal tracking. Does not
#'   affect email delivery. Example: `list(customer_id = "12345",
#'   campaign = "onboarding")`.
#' @param headers List of lists. Custom email headers. Each element should
#'   have `Name` and `Value` keys. Example: `list(list(Name = "X-Priority",
#'   Value = "High"))`. Use for custom routing, priority settings, or
#'   Message-ID override.
#' @param track_opens Logical scalar. Enable open tracking using invisible
#'   pixel. Only works with HTML emails. Provides insights on when and where
#'   emails are opened.
#' @param track_links Character scalar. Link tracking mode. Options:
#'   `"None"` (no tracking), `"HtmlAndText"` (track all links),
#'   `"HtmlOnly"` (HTML links only), `"TextOnly"` (text links only).
#'   Replaces URLs with tracking URLs that redirect to original destination.
#' @param attachments List of lists. File attachments and inline images.
#'   Each attachment should have `Name`, `Content` (base64-encoded),
#'   `ContentType`, and optionally `ContentID` (for inline images) keys.
#'   Total message size including attachments limited to 10 MB.
#'
#' @examples
#' \dontrun{
#' # Simple text Email
#' simple_email <- Email(
#'   from = "sender@example.com",
#'   to = "recipient@example.com",
#'   subject = "Hello from R",
#'   text_body = "This is a plain text email."
#' )
#'
#' # HTML Email with tracking
#' html_email <- Email(
#'   from = "sender@example.com",
#'   to = "recipient@example.com",
#'   subject = "Welcome!",
#'   html_body = "<h1>Welcome!</h1><p>Thanks for signing up.</p>",
#'   track_opens = TRUE,
#'   track_links = "HtmlOnly",
#'   tag = "welcome-email"
#' )
#'
#' # Email with multiple recipients and metadata
#' multi_email <- Email(
#'   from = "notifications@example.com",
#'   to = c("user1@example.com", "user2@example.com"),
#'   cc = "manager@example.com",
#'   subject = "Project Update",
#'   html_body = "<p>Project status update attached.</p>",
#'   metadata = list(
#'     project_id = "ABC123",
#'     department = "Engineering"
#'   ),
#'   tag = "project-updates"
#' )
#'
#' # Email with custom headers
#' priority_email <- Email(
#'   from = "urgent@example.com",
#'   to = "support@example.com",
#'   subject = "Urgent Issue",
#'   text_body = "This requires immediate attention.",
#'   headers = list(
#'     list(Name = "X-Priority", Value = "1"),
#'     list(Name = "Importance", Value = "high")
#'   )
#' )
#' }
#'
#' @seealso
#' \url{https://postmarkapp.com/developer/api/email-api#send-a-single-email}
#' for complete Postmark email API documentation
#'
#' @export
Email <- new_class(
  "Email",
  properties = list(
    from = new_property(
      class = class_character,
      validator = function(value) {
        if (length(value)) {
          validate_email(value, arg_name = "from")
        }
      }
    ),
    to = new_property(
      class = class_character,
      validator = function(value) {
        if (length(value)) {
          validate_email(value, arg_name = "to")
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
    subject = class_character,
    tag = class_character,
    html_body = class_character,
    text_body = class_character,
    reply_to = new_property(
      class = class_character,
      validator = function(value) {
        if (length(value)) {
          validate_email(value, arg_name = "reply_to")
        }
      }
    ),
    metadata = class_list,
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
    attachments = class_list
  ),
  validator = function(self) {
    has_html <- length(self@html_body) > 0
    has_text <- length(self@text_body) > 0

    if (has_html && has_text) {
      pstmrk_abort_email_body_conflict()
    }

    if (!has_html && !has_text) {
      pstmrk_abort_email_missing_body()
    }

    total_recipients <- length(self@to) + length(self@cc) + length(self@bcc)
    if (total_recipients > POSTMARK_MAX_RECIPIENTS_SINGLE) {
      pstmrk_abort_email_too_many_recipients(total_recipients)
    }
  }
)
