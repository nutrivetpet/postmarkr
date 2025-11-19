# Get Postmark API Token

Retrieves the Postmark API token from environment variables for
authentication with the Postmark API.

## Usage

``` r
get_token(env = c("prod", "test"))
```

## Arguments

- env:

  Character string specifying the environment. Either `"prod"` for
  production or `"test"` for testing. Defaults to `"prod"`.

## Value

A character string containing the API token.

## Environment Variables

The function looks for the following environment variables:

- `POSTMARK_PROD_SERVER_TOKEN` - for prod/production environment

- `POSTMARK_TEST_SERVER_TOKEN` - for test environment

## Test Mode

When using `env = "test"`, you can configure a sandbox server in
Postmark that processes emails without actually delivering them. This is
useful for development, testing, and CI/CD pipelines. For more
information, see the [Postmark Sandbox Mode
documentation](https://postmarkapp.com/developer/user-guide/sandbox-mode).

## Examples

``` r
if (FALSE) { # \dontrun{
# Set environment variable first
Sys.setenv(POSTMARK_SERVER_TOKEN = "your-token-here")

# Get prod token
token <- get_token("prod")

# Get test token
Sys.setenv(POSTMARK_TEST_SERVER_TOKEN = "your-test-token")
test_token <- get_token("test")
} # }
```
