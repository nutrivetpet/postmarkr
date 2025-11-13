# Send a single email

This function sends a single email via the Postmark API service. It
supports both HTML and plain text email formats (but not both
simultaneously) and can be used for both the "outbound" and "broadcast")
email message streams.

## Usage

``` r
email_send_single(
  from,
  to,
  msg_stream,
  subject = NULL,
  html_body = NULL,
  text_body = NULL
)
```

## Arguments

- from:

  Character scalar. Email address of the sender.

- to:

  Character vector. Email addresses of recipients (max 50).

- msg_stream:

  Character scalar. Either "outbound" or "broadcast".

- subject:

  Character scalar. Email subject line.

- html_body:

  Character scalar. HTML content of the email.

- text_body:

  Character scalar. Plain text content of the email.

## Value

A data frame or tibble (if tibble is installed) containing the response
details.

## Examples

``` r
if (FALSE) { # \dontrun{
 email_send_single(
  from = "sender@example.com",
  to = "recipient@example.com",
  msg_stream = "outbound",
  subject = "Hello from R",
  html_body = "<h1>Hello world!</h1>",
)
} # }
```
