# Print a Client Object

Pretty-prints a `Client` object, displaying all properties with their
types and values. Sensitive information (API tokens) is automatically
redacted for security.

## Arguments

- x:

  A `Client` object to print

- ...:

  Additional arguments (currently unused, included for S3 generic
  compatibility)

## Value

Invisibly returns the input object `x`, allowing for piping and chaining
operations

## Examples

``` r
if (FALSE) { # \dontrun{
# Create and print a Client object
client <- client(
  token = "your-server-token",
  message_stream = "outbound"
)
print(client)
} # }
```
