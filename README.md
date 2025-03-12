
<!-- README.md is generated from README.Rmd. Please edit that file -->

# postmarkr

<!-- badges: start -->
<!-- badges: end -->

The goal of postmarkr is to interact with the Postmark API, from R.

## Installation

You can install the development version of postmarkr like so:

``` r
pak::pak("andreranza/postmarkr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(postmarkr)
get_outbound_messages(count = 20L)
```
