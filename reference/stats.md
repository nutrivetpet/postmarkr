# Stats Object

Creates a Stats object for querying statistics from the Postmark API.
This provides a structured way to pass query parameters to Postmark
statistics API endpoints. All properties are optional. The message
stream is automatically determined from the client object and does not
need to be specified in the params.

## Usage

``` r
stats(tag = character(), fromdate = character(), todate = character())

Stats(tag = character(0), fromdate = character(0), todate = character(0))
```

## Arguments

- tag:

  character. Optional tag to filter statistics by. Tags are used to
  categorize emails for detailed tracking and reporting.

- fromdate:

  character. Optional start date for the statistics query in YYYY-MM-DD
  format (e.g., "2024-01-01").

- todate:

  character. Optional end date for the statistics query in YYYY-MM-DD
  format (e.g., "2024-12-31").

## See also

<https://postmarkapp.com/developer/api/stats-api> for Postmark Stats API
documentation

## Examples

``` r
if (FALSE) { # \dontrun{
# Create Stats parameters with all fields
stats <- stats(
  tag = "welcome-email",
  fromdate = "2024-01-01",
  todate = "2024-01-31"
)

# Create Stats parameters with only date range
stats <- stats(
  fromdate = "2024-01-01",
  todate = "2024-01-31"
)

# Create empty Stats parameters
stats <- stats()
} # }
```
