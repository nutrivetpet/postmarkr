# postmarkr

The goal of postmarkr is to interact with the [Postmark
API](https://postmarkapp.com/developer), from R.

It is an independent, community-developed R package for the
[Postmark](https://postmarkapp.com) email service (**not created by or
affiliated with Postmark**).

## Installation

You can install the development version of postmarkr like so:

``` r
pak::pak("nutrivetpet/postmarkr")
```

## Usage Patterns

postmarkr provides a clean, object-oriented API for sending emails
through Postmark. The package supports two message types (email and
template) and two delivery modes (individual and batch):

|                | [`email()`](https://nutrivetpet.github.io/postmarkr/reference/email.md) | [`template()`](https://nutrivetpet.github.io/postmarkr/reference/template.md) |
|----------------|-------------------------------------------------------------------------|-------------------------------------------------------------------------------|
| **Individual** | One email with HTML/text body                                           | One email using template ID                                                   |
| **Batch**      | Multiple custom emails (500+)                                           | Multiple template emails (500+)                                               |

### Individual Email (Custom Content)

Send a single email with custom HTML or text content:

``` r
library(postmarkr)

# Create client
client <- client(
  token = Sys.getenv("POSTMARK_SERVER_TOKEN"),
  message_stream = "outbound"
)

# Create and send email
email <- email(
  from = "sender@example.com",
  to = "recipient@example.com",
  subject = "Welcome to postmarkr",
  html_body = "<h1>Hello!</h1><p>Welcome to our service.</p>",
  track_opens = TRUE
)

result <- send(client, email)
```

### Individual Template Email

Send a single email using a predefined Postmark template:

``` r
template <- template(
  from = "notifications@example.com",
  to = "user@example.com",
  id = 36620093L,
  template_model = list(
    user_name = "Alice",
    company_name = "ACME Corp"
  ),
  track_opens = TRUE
)

result <- send(client, template)
```

### Batch Emails

Send multiple custom emails efficiently (automatically chunks into
groups of 500):

``` r
# Create multiple personalized emails
emails <- lapply(recipients, function(recipient) {
  email(
    from = "sender@example.com",
    to = recipient,
    html_body = "<p>Hi %s, welcome aboard!</p>"
  )
})

# Wrap in batch and send
batch <- batch(messages = emails)
result <- send(client, batch)
```

### Batch Templates

Send multiple template emails (e.g., newsletters, notifications):

``` r
# Create template emails for each recipient
templates <- lapply(recipients, function(recipient) {
  template(
    from = "newsletter@example.com",
    to = recipient,
    id = 36620093L,
    template_model = list(
      user_name = "Alice",
      company_name = "ACME Corp"
    )
  )
})

# Wrap in batch and send
batch <- batch(messages = templates)
result <- send(client, batch)
```

## Features

- **Custom Emails**: Compose emails with HTML or text using
  [`email()`](https://nutrivetpet.github.io/postmarkr/reference/email.md)
- **Template Emails**: Use Postmark templates with variable substitution
  via
  [`template()`](https://nutrivetpet.github.io/postmarkr/reference/template.md)
- **Batch Sending**: Efficiently send 500+ emails with
  [`batch()`](https://nutrivetpet.github.io/postmarkr/reference/batch.md)
  (automatic chunking)
- **Tracking**: Built-in support for open and click tracking

**Note:** Postmark API coverage is limited. See the
[documentation](https://nutrivetpet.github.io/postmarkr/) for all
available features.
