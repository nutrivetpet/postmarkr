# Get Statistics from Postmark

Generic function to retrieve statistics from various Postmark API
endpoints. This provides a flexible way to query different types of
statistics using the same interface.

## Usage

``` r
stats_get(client, params, ..., endpoint = NULL)
```

## Arguments

- client:

  A Client object created with
  [`client()`](https://nutrivetpet.github.io/postmarkr/reference/client.md).

- params:

  Optional Stats parameters object created with
  [`Stats()`](https://nutrivetpet.github.io/postmarkr/reference/Stats.md).
  If not provided, no query parameters will be sent.

- ...:

  Additional arguments passed to methods.

- endpoint:

  A character argument specifying the Stats endpoint path (e.g.,
  "overview", "sends", "bounces", "opens/emailclients", "clicks",
  "clicks/browserfamilies"). The "/stats/" prefix and message stream
  will be added automatically.

## Value

A Response object containing the statistics data.

## See also

<https://postmarkapp.com/developer/api/stats-api> for Postmark Stats API
documentation

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a client
client <- Client(
  token = "your-server-token",
  message_stream = "outbound"
)

# Get sent counts with date range
params <- Stats(
  fromdate = "2024-01-01",
  todate = "2024-01-31"
)
stats_get(client, params)

# Get open counts filtered by tag
params <- Stats(
  tag = "welcome-email",
  fromdate = "2024-01-01",
  todate = "2024-01-31"
)
stats_get(client, params, "opens")

# Get click statistics by browser family
stats_get(client, params, "clicks/browserfamilies")
} # }
```
