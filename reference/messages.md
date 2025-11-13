# Retrieve Outbound Messages

Fetches a list of outbound messages from the Postmark API. This function
allows you to retrieve messages with pagination support and additional
filtering options.

## Usage

``` r
outbound_messages_fetch(count, offset = 0L, ...)
```

## Arguments

- count:

  An integer specifying the number of messages to retrieve. Must be
  positive and not exceed 500.

- offset:

  An integer specifying the number of messages to skip. Defaults to 0.
  The sum of count and offset must not exceed 10'000.

- ...:

  Additional query parameters to filter results. See
  <https://postmarkapp.com/developer/api/messages-api#outbound-messages>
  for supported parameters (e.g., `recipient`, `tag`, `status`).

## Value

A list of messages.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get the 10 most recent outbound messages
recent_messages <- outbound_messages_fetch(10L)

# Skip the first 50 messages and get the next 20
next_page <- outbound_messages_fetch(20L, offset = 50L)

# Filter messages by recipient
to_john <- outbound_messages_fetch(
  count = 25L,
  recipient = "john@example.com"
)

# Filter by tag and status
newsletter_sent <- outbound_messages_fetch(
  count = 100L,
  tag = "newsletter",
  status = "sent"
)
} # }
```
