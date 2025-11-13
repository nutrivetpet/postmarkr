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

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(postmarkr)
dat <- template_send_email_batch(
  from = "xyz@mail.com",
  to = c(
    "abcd@mail.com",
    "efgh@mail.com"
  ),
  id = 36620093L,
  template_model = list(
    template_var_01 = "nutrivetpet",
    template_var_02 = "www.nutrivetpet.com"
  ),
  msg_stream = "broadcast",
  tag = "test",
  track_opens = TRUE
)
```

## Features

- **Single Email Delivery**: Send individual emails with
  [`email_send_single()`](https://andreranza.github.io/postmarkr/reference/email.md)
- **Template-Based Emails**: Send templated emails with
  [`template_send_email()`](https://andreranza.github.io/postmarkr/reference/template_send_email.md)
  and
  [`template_send_email_batch()`](https://andreranza.github.io/postmarkr/reference/template_send_email_batch.md)
- **Email Templates**: List available email templates with
  [`template_list()`](https://andreranza.github.io/postmarkr/reference/template_list.md)
- **Message Logs**: Retrieve outbound messages with
  [`outbound_messages_collect()`](https://andreranza.github.io/postmarkr/reference/outbound_messages_collect.md)
  and
  [`outbound_messages_fetch()`](https://andreranza.github.io/postmarkr/reference/messages.md)
- **Delivery Statistics**: Track email performance with
  [`stats_outbound_overview()`](https://andreranza.github.io/postmarkr/reference/stats.md)

**Note:** API coverage is still pretty limited compared to all available
endpoints.
