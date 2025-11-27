# Check Postmark Service Status

Queries the Postmark status API to check if the service is operational.
This function does not require authentication and can be used to verify
that Postmark's infrastructure is running before attempting to send
emails.

## Usage

``` r
status_get(timeout = 10)
```

## Arguments

- timeout:

  numeric. Request timeout in seconds. Defaults to 10 seconds.

## Value

A named list with a state element that indicates whether the service is
"operational", "degraded" or "under_maintenance".

## See also

<https://status.postmarkapp.com/api> for the Postmark status page

## Examples

``` r
if (FALSE) { # \dontrun{
# Check if Postmark is operational
status <- status_get()
print(status)

# Check with custom timeout
status <- status_get(timeout = 5)
} # }
```
