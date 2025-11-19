# Batch Object - Collection of Email or Template Objects

Creates a Batch object for sending multiple emails or templates via the
Postmark batch API. This wraps a collection of messages (either `Email`
or `Template` objects) and manages the complexities of Postmark's batch
API:

- **Homogeneity validation**: All messages must be the same type (either
  all `Email` or all `Template`)

- **Automatic chunking**: Splits large batches into API-compliant chunks
  of 500 messages (Postmark's limit)

- **Configurable chunk size**: Allows smaller chunks for testing or rate
  limiting

**Batch API Endpoints:**

- For `Email` messages: `/email/batch`

- For `Template` messages: `/email/batchWithTemplates`

**Important Limitations:**

- Maximum 500 messages per API call (handled automatically via chunking)

- Maximum 50 MB total payload size per chunk

- Each message still subject to individual limits (50 recipients, 10 MB)

## Usage

``` r
batch(messages, chunk_size = POSTMARK_MAX_BATCH_SIZE)

Batch(messages = list(), chunk_size = 500L)
```

## Arguments

- messages:

  List of `Email` or `Template` objects. All must be the same class. Use
  [`lapply()`](https://rdrr.io/r/base/lapply.html) or similar to create
  multiple messages programmatically.

- chunk_size:

  Integer scalar. Maximum messages per API call. Default is 500
  (Postmark's limit). Set lower for testing (e.g., 50) or conservative
  rate limiting (e.g., 100). Must be positive and not exceed 500.

## See also

- <https://postmarkapp.com/developer/api/email-api#send-batch-emails>
  for Email batch API

- <https://postmarkapp.com/developer/api/templates-api#send-batch-with-templates>
  for Template batch API

- [`email()`](https://nutrivetpet.github.io/postmarkr/reference/email.md)
  for single email messages

- [`template()`](https://nutrivetpet.github.io/postmarkr/reference/template.md)
  for template-based messages

- [`send()`](https://nutrivetpet.github.io/postmarkr/reference/send.md)
  for sending batches

## Examples

``` r
if (FALSE) { # \dontrun{
# Batch of regular emails (e.g., 1000 personalized emails)
emails <- lapply(1:1000, function(i) {
  email(
    from = "sender@example.com",
    to = sprintf("user%d@example.com", i),
    subject = sprintf("Welcome User %d", i),
    html_body = sprintf("<p>Hello User %d!</p>", i)
  )
})
my_batch <- batch(messages = emails)

# Send batch
client <- client(token = "<token>", message_stream = "outbound")
result <- send(client, my_batch)

# Batch of template emails
recipients <- c("alice@example.com", "bob@example.com", "charlie@example.com")
templates <- lapply(recipients, function(email) {
  template(
    from = "notifications@example.com",
    to = email,
    id = 12345678L,
    template_model = list(
      company_name = "ACME Corp"
    )
  )
})
my_batch <- batch(messages = templates)
send(client, my_batch)
} # }
```
