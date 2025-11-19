# Client Object

Creates a Client object for interacting with the Postmark API. This
provides a structured way to configure and use the Postmark API. Before
creating an instance, ensure you have:

- A valid Postmark Server API Token

- Verified sender signatures or domains in your Postmark account

## Usage

``` r
client(token, message_stream, timeout = 60, verbose = FALSE)

Client(
  token = character(0),
  message_stream = character(0),
  base_url = "https://api.postmarkapp.com",
  timeout = 60,
  verbose = FALSE
)
```

## Arguments

- token:

  character. Your Postmark Server API Token. This token authenticates
  your requests to the Postmark API.

- message_stream:

  character. The message stream to use for sending emails. Must be
  either `"broadcast"` (for newsletters and marketing emails) or
  `"outbound"` (for transactional one-to-one triggered emails).

- timeout:

  numeric. Request timeout in seconds for API calls. 60 seconds by
  default.

- verbose:

  logical. Whether to pass
  [`httr2::req_verbose()`](https://httr2.r-lib.org/reference/req_verbose.html)
  to the request.

- base_url:

  character. The base URL for the Postmark API. Defaults to the standard
  Postmark API endpoint (`https://api.postmarkapp.com`). **You should
  not need to change this.**

## Value

A Client object for making API calls to Postmark

## See also

<https://postmarkapp.com/developer/api/overview> for Postmark API
documentation

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a client for transactional emails
client <- client(
  token = "your-server-token-here",
  message_stream = "outbound",
  timeout = 30
)

# Create a client for broadcast emails
broadcast_client <- client(
  token = "your-server-token-here",
  message_stream = "broadcast"
)
} # }
```
