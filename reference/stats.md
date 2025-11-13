# Get Overview of Outbound Messages

Retrieves a summary of outbound message statistics from the Postmark
API. This function fetches aggregated metrics about messages sent
through your Postmark account.

## Usage

``` r
stats_outbound_overview(...)
```

## Arguments

- ...:

  Additional arguments passed to the underlying request functions.

## Value

A list containing outbound message statistics as returned by the
Postmark API.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get outbound message statistics
stats <- stats_outbound_overview()
} # }
```
