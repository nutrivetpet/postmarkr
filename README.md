
<!-- README.md is generated from README.Rmd. Please edit that file -->

# postmarkr <img src="man/figures/logo.png" align="right" height="138" />

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/andreranza/postmarkr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/andreranza/postmarkr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/nutrivetpet/postmarkr/graph/badge.svg)](https://app.codecov.io/gh/nutrivetpet/postmarkr)
<!-- badges: end -->

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
  `email_send_single()`
- **Template-Based Emails**: Send templated emails with
  `template_send_email()` and `template_send_email_batch()`
- **Email Templates**: List available email templates with
  `template_list()`
- **Message Logs**: Retrieve outbound messages with
  `outbound_messages_collect()` and `outbound_messages_fetch()`
- **Delivery Statistics**: Track email performance with
  `stats_outbound_overview()`

**Note:** API coverage is still pretty limited compared to all available
endpoints.
