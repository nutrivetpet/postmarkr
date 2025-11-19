# Template Object

Creates a Template object for sending template-based emails via the
Postmark API. This provides a structured way to send emails using
predefined Postmark templates. It supports:

- Template variable substitution via `template_model`

- Multiple recipients via To, Cc, and Bcc fields (max 50 combined)

- Inline CSS processing for better email client compatibility

- Open and click tracking configuration

- Email categorization via tags

- File attachments and custom headers

- Metadata for internal tracking

## Usage

``` r
template(
  template_model,
  from = character(0),
  to = character(0),
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
)

Template(
  from = character(0),
  to = character(0),
  id = integer(0),
  alias = character(0),
  template_model = list(),
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
)
```

## Arguments

- template_model:

  List. **Required.** Named list of variables to populate in the
  template. Keys must match template variable names. Can include nested
  lists for complex data structures. If the template has no
  placeholders, pass an empty list:
  [`list()`](https://rdrr.io/r/base/list.html). Example with variables:
  `list(user_name = "John", company = list(name = "ACME"))`.

- from:

  Character scalar. **Required.** Sender email address (must be verified
  in Postmark). Supports name formatting:
  `"John Doe <email@example.com>"`

- to:

  Character vector. **Required.** Recipient email addresses. Multiple
  recipients allowed (max 50 total across To, Cc, Bcc)

- id:

  Integer scalar. The template ID in Postmark. Must be a positive
  integer corresponding to an existing template in your account.
  Typically an 8-digit number (e.g., 12345678). **Mutually exclusive
  with `alias`**.

- alias:

  Character scalar. The template alias in Postmark. A named identifier
  for your template (e.g., "welcome-email", "password-reset").
  **Mutually exclusive with `id`**.

- cc:

  Character vector. Carbon copy recipients. Visible to all recipients.

- bcc:

  Character vector. Blind carbon copy recipients. Hidden from other
  recipients.

- inline_css:

  Logical scalar. Whether to process CSS in `<style>` tags into inline
  style attributes. Improves compatibility with email clients. Default
  is TRUE for better rendering.

- tag:

  Character scalar. Category tag for statistics and filtering. One tag
  per message, maximum 1000 characters. Examples: "welcome-email",
  "password-reset", "invoice".

- reply_to:

  Character scalar. Reply-To address for responses.

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

- metadata:

  List. Key-value pairs for internal tracking. Does not affect email
  delivery. Example:
  `list(customer_id = "12345", campaign = "onboarding")`.

## Limitations

- Maximum 50 total recipients (To, Cc, Bcc combined)

- Maximum 10 MB total message size (including attachments)

- One tag per message, maximum 1000 characters

## See also

<https://postmarkapp.com/developer/api/templates-api#send-email-with-template>
for complete Postmark template API documentation

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple template email using template ID
simple_template <- template(
  from = "sender@example.com",
  to = "recipient@example.com",
  id = 12345678L,
  template_model = list(name = "John", message = "Welcome!")
)

# Template email using template alias
alias_template <- template(
  from = "sender@example.com",
  to = "recipient@example.com",
  alias = "welcome-email",
  template_model = list(name = "John", message = "Welcome!")
)

# Template with no placeholders (empty model required)
no_placeholders <- template(
  from = "sender@example.com",
  to = "recipient@example.com",
  id = 11111111L,
  template_model = list()
)

# Template with tracking, attachments, and metadata
full_template <- template(
  from = "notifications@example.com",
  to = c("user1@example.com", "user2@example.com"),
  cc = "manager@example.com",
  id = 67890123L,
  template_model = list(
    order_id = "ORD-789",
    status = "Completed"
  ),
  inline_css = TRUE,
  tag = "order-confirmation",
  reply_to = "support@example.com",
  headers = list(
    list(Name = "X-Priority", Value = "High")
  ),
  track_opens = TRUE,
  track_links = "HtmlOnly",
  attachments = list(
    list(
      Name = "invoice.pdf",
      Content = "base64content",
      ContentType = "application/pdf"
    )
  ),
  metadata = list(
    customer_id = "12345",
    order_type = "subscription"
  )
)
} # }
```
