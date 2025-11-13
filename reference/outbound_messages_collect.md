# Collect all outbound email messages

This function retrieves all outbound messages by making multiple API
calls as needed. It handles pagination automatically by generating
appropriate batches of count and offset values.

## Usage

``` r
outbound_messages_collect(quiet = TRUE, ...)
```

## Arguments

- quiet:

  Logical. If FALSE, displays an informational message about the total
  number of emails. Default is TRUE (no messages).

- ...:

  Additional arguments passed to
  [`outbound_messages_fetch()`](https://andreranza.github.io/postmarkr/reference/messages.md).

## Value

A data frame or tibble (if tibble is installed) containing all retrieved
outbound messages.

The function works by first getting an overview of outbound emails to
determine the total count. It then divides this into appropriate batches
for pagination and makes multiple API calls to retrieve all messages.
Results are combined into a single data frame or tibble.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get all outbound messages with default settings
messages <- outbound_messages_collect()

# Get messages and display count information
messages <- outbound_messages_collect(quiet = FALSE)
} # }
```
