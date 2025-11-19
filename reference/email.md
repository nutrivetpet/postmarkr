# Email Object

Creates an Email object for sending emails via the Postmark API. This
provides a structured way to compose emails for sending through
Postmark. It supports:

- HTML and plain text content (mutually exclusive)

- Multiple recipients via To, Cc, and Bcc fields (max 50 combined)

- Custom email headers for routing and priority

- Metadata for internal tracking

- File attachments and inline images

- Open and click tracking configuration

- Email categorization via tags

## Usage

``` r
email(
  from = character(0),
  to = character(0),
  subject = character(0),
  html_body = character(0),
  text_body = character(0),
  cc = character(0),
  bcc = character(0),
  tag = character(0),
  reply_to = character(0),
  metadata = list(),
  headers = list(),
  track_opens = logical(0),
  track_links = character(0),
  attachments = list()
)

Email(
  from = character(0),
  to = character(0),
  cc = character(0),
  bcc = character(0),
  subject = character(0),
  tag = character(0),
  html_body = character(0),
  text_body = character(0),
  reply_to = character(0),
  metadata = list(),
  headers = list(),
  track_opens = logical(0),
  track_links = character(0),
  attachments = list()
)
```

## Arguments

- from:

  Character scalar. **Required.** Sender email address (must be verified
  in Postmark). Supports name formatting:
  `"John Doe <email@example.com>"`

- to:

  Character vector. **Required.** Recipient email addresses. Multiple
  recipients allowed (max 50 total across To, Cc, Bcc)

- subject:

  Character scalar. Email subject line.

- html_body:

  Character scalar. HTML content of the email. Use for rich formatting,
  images, and styling. **Mutually exclusive with `text_body`.** Maximum
  5 MB.

- text_body:

  Character scalar. Plain text content of the email. Use for simple
  text-only emails. **Mutually exclusive with `html_body`.** Maximum 5
  MB.

- cc:

  Character vector. Carbon copy recipients. Visible to all recipients.

- bcc:

  Character vector. Blind carbon copy recipients. Hidden from other
  recipients.

- tag:

  Character scalar. Category tag for statistics and filtering. One tag
  per message, maximum 1000 characters. Examples: "welcome-email",
  "password-reset", "invoice".

- reply_to:

  Character scalar. Reply-To address for responses.

- metadata:

  List. Key-value pairs for internal tracking. Does not affect email
  delivery. Example:
  `list(customer_id = "12345", campaign = "onboarding")`.

- headers:

  List of lists. Custom email headers. Each element should have `Name`
  and `Value` keys. Example:
  `list(list(Name = "X-Priority", Value = "High"))`. Use for custom
  routing, priority settings, or Message-ID override.

- track_opens:

  Logical scalar. Enable open tracking using invisible pixel. Only works
  with HTML emails. Provides insights on when and where emails are
  opened.

- track_links:

  Character scalar. Link tracking mode. Options: `"None"` (no tracking),
  `"HtmlAndText"` (track all links), `"HtmlOnly"` (HTML links only),
  `"TextOnly"` (text links only). Replaces URLs with tracking URLs that
  redirect to original destination.

- attachments:

  List of lists. File attachments and inline images. Each attachment
  should have `Name`, `Content` (base64-encoded), `ContentType`, and
  optionally `ContentID` (for inline images) keys. Total message size
  including attachments limited to 10 MB.

## Limitations

- Maximum 50 total recipients (To, Cc, Bcc combined)

- Maximum 10 MB total message size (including attachments)

- One tag per message, maximum 1000 characters

## See also

<https://postmarkapp.com/developer/api/email-api#send-a-single-email>
for complete Postmark email API documentation

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple text Email
simple_email <- email(
  from = "sender@example.com",
  to = "recipient@example.com",
  subject = "Hello from R",
  text_body = "This is a plain text email."
)

# HTML Email with tracking
html_email <- email(
  from = "sender@example.com",
  to = "recipient@example.com",
  subject = "Welcome!",
  html_body = "<h1>Welcome!</h1><p>Thanks for signing up.</p>",
  track_opens = TRUE,
  track_links = "HtmlOnly",
  tag = "welcome-email"
)

# Email with multiple recipients and metadata
multi_email <- email(
  from = "notifications@example.com",
  to = c("user1@example.com", "user2@example.com"),
  cc = "manager@example.com",
  subject = "Project Update",
  html_body = "<p>Project status update attached.</p>",
  metadata = list(
    project_id = "ABC123",
    department = "Engineering"
  ),
  tag = "project-updates"
)

# Email with custom headers
priority_email <- email(
  from = "urgent@example.com",
  to = "support@example.com",
  subject = "Urgent Issue",
  text_body = "This requires immediate attention.",
  headers = list(
    list(Name = "X-Priority", Value = "1"),
    list(Name = "Importance", Value = "high")
  )
)

} # }
```
