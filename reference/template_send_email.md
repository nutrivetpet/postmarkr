# Send an Email Using a Template

Sends an email using a predefined template.

## Usage

``` r
template_send_email(
  from,
  to,
  id,
  template_model,
  msg_stream,
  tag = NULL,
  track_opens = FALSE
)
```

## Arguments

- from:

  Character scalar. Email address of the sender.

- to:

  Character vector. Email addresses of recipients (max 50).

- id:

  A single integer. The template ID in Postmark.

- template_model:

  A named list. Variables to be populated in the template.

- msg_stream:

  Character scalar. Either "outbound" or "broadcast".

- tag:

  A single character string. Optional tag for categorizing the email.
  Maximum 1000 characters. Default is NULL.

- track_opens:

  A logical value. Whether to track when recipients open the email.
  Default is FALSE.

## Value

A data frame or tibble (if tibble is installed) containing the response
details, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
template_send_email(
  from = "sender@example.com",
  to = c("recipient1@example.com", "recipient2@example.com"),
  id = 12345,
  template_model = list(
    name = "John",
    message = "Hello from Postmark!"
  ),
  msg_stream = "outbound",
  tag = "welcome-email",
  track_opens = TRUE
)
} # }
```
