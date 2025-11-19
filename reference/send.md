# Send Email, Template and Batch Objects

Generic function to send emails through the Postmark API. This function
dispatches to the appropriate method based on the message type
([Email](https://nutrivetpet.github.io/postmarkr/reference/email.md),
[Template](https://nutrivetpet.github.io/postmarkr/reference/template.md),
[Batch](https://nutrivetpet.github.io/postmarkr/reference/batch.md)).

## Usage

``` r
send(client, message, ...)
```

## Arguments

- client:

  A
  [Client](https://nutrivetpet.github.io/postmarkr/reference/client.md)
  client object.

- message:

  An
  [Email](https://nutrivetpet.github.io/postmarkr/reference/email.md),
  [Template](https://nutrivetpet.github.io/postmarkr/reference/template.md)
  or [Batch](https://nutrivetpet.github.io/postmarkr/reference/batch.md)
  object to send.

- ...:

  Additional arguments passed to methods.

## Value

A `Response` S7 object with the following properties:

- data:

  List containing API response data including `MessageID`,
  `SubmittedAt`, `To`, `ErrorCode`, and `Message` fields

- status:

  HTTP status code (200 for success)

- request:

  The httr2 request object used for the API call

- response:

  The httr2 response object from the API

- success:

  Logical indicating if the email was sent successfully

## See also

- [Client](https://nutrivetpet.github.io/postmarkr/reference/client.md)
  for creating an API client

- [Email](https://nutrivetpet.github.io/postmarkr/reference/email.md)
  for creating email objects

- [Template](https://nutrivetpet.github.io/postmarkr/reference/template.md)
  for creating template objects

- <https://postmarkapp.com/developer/api/email-api#send-a-single-email>
  for Postmark email API documentation

- <https://postmarkapp.com/developer/api/templates-api#send-email-with-template>
  for Postmark template API documentation

## Examples

``` r
if (FALSE) { # \dontrun{
# Create client
client <- Client(
  token = "your-server-token",
  message_stream = "outbound"
)

# Send a regular email
email <- Email(
  from = "sender@example.com",
  to = "recipient@example.com",
  subject = "Hello from postmarkr",
  text_body = "This is a test email sent via R."
)

response <- send(client, email)

# Check if successful
if (response@success) {
  cat("Email sent successfully!\n")
  cat("Message ID:", response@data$MessageID, "\n")
  cat("Submitted at:", response@data$SubmittedAt, "\n")
}

# Send HTML email with tracking
html_email <- Email(
  from = "notifications@example.com",
  to = "user@example.com",
  subject = "Welcome to our service",
  html_body = "<h1>Welcome!</h1><p>Thanks for signing up.</p>",
  track_opens = TRUE,
  track_links = "HtmlOnly",
  tag = "welcome-email"
)

response <- send(client, html_email)

# Send a template email
template <- Template(
  from = "sender@example.com",
  to = "recipient@example.com",
  id = 12345678L,
  template_model = list(name = "John", order_id = "ORD-123")
)

response <- send(client, template)
} # }
```
