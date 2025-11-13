# Send Batch Email Using a Template

Send email to more than 500 recipients, sending multiple POST requests
via `req_perform_sequential()`.

## Usage

``` r
template_send_email_batch(
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
