#' Email - Single email message
#'
#' @description
#' An S7 class representing a single email message to be sent via the Postmark
#' API. This class encapsulates all the properties needed to compose and send
#' an email, including content, recipients, tracking settings, and attachments.
#'
#' @details
#' The `email` class provides a structured way to compose emails for sending
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
#' @section Properties:
#'
#' **Required:**
#' \describe{
#'   \item{from}{Character scalar. Sender email address (must be verified in
#'     Postmark). Supports name formatting: `"John Doe <email@example.com>"`}
#'   \item{to}{Character vector. Recipient email addresses. Multiple recipients
#'     allowed (max 50 total across To, Cc, Bcc)}
#' }
#'
#' **Content (one required):**
#' \describe{
#'   \item{html_body}{Character scalar. HTML content of the email. Use for
#'     rich formatting, images, and styling. Mutually exclusive with
#'     `text_body`. Maximum 5 MB.}
#'   \item{text_body}{Character scalar. Plain text content of the email.
#'     Use for simple text-only emails. Mutually exclusive with `html_body`.
#'     Maximum 5 MB.}
#' }
#'
#' **Optional:**
#' \describe{
#'   \item{subject}{Character scalar. Email subject line.}
#'   \item{cc}{Character vector. Carbon copy recipients. Visible to all
#'     recipients.}
#'   \item{bcc}{Character vector. Blind carbon copy recipients. Hidden from
#'     other recipients.}
#'   \item{reply_to}{Character scalar. Reply-To address for responses.}
#'   \item{tag}{Character scalar. Category tag for statistics and filtering.
#'     One tag per message, maximum 1000 characters. Examples: "welcome-email",
#'     "password-reset", "invoice".}
#'   \item{metadata}{List. Key-value pairs for internal tracking. Does not
#'     affect email delivery. Example: `list(customer_id = "12345",
#'     campaign = "onboarding")`.}
#'   \item{headers}{List of lists. Custom email headers. Each element should
#'     have `Name` and `Value` keys. Example: `list(list(Name = "X-Priority",
#'     Value = "High"))`. Use for custom routing, priority settings, or
#'     Message-ID override.}
#'   \item{track_opens}{Logical scalar. Enable open tracking using invisible
#'     pixel. Only works with HTML emails. Provides insights on when and where
#'     emails are opened.}
#'   \item{track_links}{Character scalar. Link tracking mode. Options:
#'     `"None"` (no tracking), `"HtmlAndText"` (track all links),
#'     `"HtmlOnly"` (HTML links only), `"TextOnly"` (text links only).
#'     Replaces URLs with tracking URLs that redirect to original destination.}
#'   \item{attachments}{List of lists. File attachments and inline images.
#'     Each attachment should have `Name`, `Content` (base64-encoded),
#'     `ContentType`, and optionally `ContentID` (for inline images) keys.
#'     Total message size including attachments limited to 10 MB.}
#' }
#'
#' @examples
#' \dontrun{
#' # Simple text email
#' simple_email <- email(
#'   from = "sender@example.com",
#'   to = "recipient@example.com",
#'   subject = "Hello from R",
#'   text_body = "This is a plain text email."
#' )
#'
#' # HTML email with tracking
#' html_email <- email(
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
#' multi_email <- email(
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
#' priority_email <- email(
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
email <- new_class(
  "email",
  properties = list(
    from = class_character,
    to = class_character,
    cc = class_character,
    bcc = class_character,
    subject = class_character,
    tag = class_character,
    html_body = class_character,
    text_body = class_character,
    reply_to = class_character,
    metadata = class_list,
    headers = class_list,
    track_opens = class_logical,
    track_links = class_character,
    attachments = class_list
  ),
  validator = function(self) {
    # Check html_body and text_body are mutually exclusive and at least one exists
    has_html <- length(self@html_body) > 0
    has_text <- length(self@text_body) > 0

    if (has_html && has_text) {
      email_abort_body_conflict()
    }

    if (!has_html && !has_text) {
      email_abort_missing_body()
    }

    # Check maximum recipients (To, Cc, Bcc combined <= 50)
    total_recipients <- length(self@to) + length(self@cc) + length(self@bcc)
    if (total_recipients > POSTMARK_MAX_RECIPIENTS_SINGLE) {
      email_abort_too_many_recipients(total_recipients)
    }
  }
)

#' Send a single email
#'
#' This function sends a single email via the Postmark API service. It supports
#' both HTML and plain text email formats (but not both simultaneously) and can
#' be used for both the "outbound" and "broadcast" email message streams.
#'
#' @param from Character scalar. Email address of the sender.
#' @param to Character vector. Email addresses of recipients (max 50).
#' @param msg_stream Character scalar. Either "outbound" or "broadcast".
#' @param subject Character scalar. Email subject line.
#' @param html_body Character scalar. HTML content of the email.
#' @param text_body Character scalar. Plain text content of the email.
#'
#' @return A data frame or tibble (if tibble is installed) containing the
#'   response details.
#'
#' @rdname email
#'
#' @examples
#' \dontrun{
#'  email_send_single(
#'   from = "sender@example.com",
#'   to = "recipient@example.com",
#'   msg_stream = "outbound",
#'   subject = "Hello from R",
#'   html_body = "<h1>Hello world!</h1>",
#' )
#' }
#' @export
email_send_single <- function(
  from,
  to,
  msg_stream,
  subject = NULL,
  html_body = NULL,
  text_body = NULL
) {
  email_send_single_impl(
    from = from,
    to = to,
    msg_stream = msg_stream,
    subject = subject,
    html_body = html_body,
    text_body = text_body,
    env = "live"
  )
}

email_send_single_impl <- function(
  from,
  to,
  msg_stream,
  subject = NULL,
  html_body = NULL,
  text_body = NULL,
  env = c("live", "test")
) {
  recipients_error <- sprintf(
    "`to` must have %d or fewer recipients",
    POSTMARK_MAX_RECIPIENTS_SINGLE
  )
  stopifnot(
    "`from` must be a single character string" = is_scalar_character(from),
    "`to` must be a character vector" = is_character(to),
    recipients_error = length(to) <= POSTMARK_MAX_RECIPIENTS_SINGLE
  )

  msg_stream <- arg_match(msg_stream, c("outbound", "broadcast"))

  if (!is.null(subject)) {
    stopifnot(
      "`subject` must be a single character string" = is_scalar_character(
        subject
      )
    )
  }

  has_html <- !is.null(html_body)
  has_text <- !is.null(text_body)

  if (has_html) {
    stopifnot(
      "`html_body` must be a single character string" = is_scalar_character(
        html_body
      )
    )
  }

  if (has_text) {
    stopifnot(
      "`text_body` must be a single character string" = is_scalar_character(
        text_body
      )
    )
  }

  if (has_html && has_text) {
    abort(
      "Cannot provide both `html_body` and `text_body`.",
      class = "too_many_args"
    )
  }

  if (!has_html && !has_text) {
    abort(
      "Must provide either `html_body` or `text_body`.",
      class = "missing_args"
    )
  }

  to <- paste0(to, collapse = ", ")

  bdy <- list(
    From = from,
    To = to,
    Subject = subject,
    MessageStream = msg_stream
  )

  if (is.null(text_body)) {
    bdy[["HtmlBody"]] <- html_body
  } else {
    bdy[["TextBody"]] <- text_body
  }

  req <-
    build_req("email", "POST", env) |>
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

  dat
}
