#' @include postmarkr.R
NULL

#' Batch - Collection of Email or Template Messages
#'
#' @description
#' An S7 class representing a batch of email or template messages to be sent
#' via Postmark's batch API. This class handles chunking, validation, and
#' efficient sending of multiple messages in a single operation.
#'
#' @details
#' The `Batch` class wraps a collection of messages (either `Email` or
#' `Template` objects) and manages the complexities of Postmark's batch API:
#'
#' \itemize{
#'   \item **Homogeneity validation**: All messages must be the same type
#'     (either all `Email` or all `Template`)
#'   \item **Automatic chunking**: Splits large batches into API-compliant
#'     chunks of 500 messages (Postmark's limit)
#'   \item **Type-agnostic design**: Works seamlessly with both message types
#'   \item **Configurable chunk size**: Allows smaller chunks for testing or
#'     rate limiting
#' }
#'
#' **Batch API Endpoints:**
#' \itemize{
#'   \item For `Email` messages: `/email/batch`
#'   \item For `Template` messages: `/email/batchWithTemplates`
#' }
#'
#' **Important Limitations:**
#' \itemize{
#'   \item Maximum 500 messages per API call (handled automatically via chunking)
#'   \item Maximum 50 MB total payload size per chunk
#'   \item Each message still subject to individual limits (50 recipients, 10 MB)
#'   \item All messages in batch must be same type (Email or Template)
#' }
#'
#' @param messages List of `Email` or `Template` objects. All must be the same
#'   class. Use `lapply()` or similar to create multiple messages programmatically.
#' @param chunk_size Integer scalar. Maximum messages per API call. Default is
#'   500 (Postmark's limit). Set lower for testing (e.g., 50) or conservative
#'   rate limiting (e.g., 100). Must be positive and not exceed 500.
#'
#' @examples
#' \dontrun{
#' # Batch of regular emails (e.g., 1000 personalized emails)
#' emails <- lapply(1:1000, function(i) {
#'   Email(
#'     from = "sender@example.com",
#'     to = sprintf("user%d@example.com", i),
#'     subject = sprintf("Welcome User %d", i),
#'     html_body = sprintf("<p>Hello User %d!</p>", i)
#'   )
#' })
#' batch <- Batch(messages = emails)
#'
#' # Check batch metadata
#' cat(sprintf(
#'   "Batch contains %d emails in %d chunks\n",
#'   batch_size(batch),
#'   batch_chunk_count(batch)
#' ))
#'
#' # Send batch
#' client <- PostmarkClient(token = "<token>")
#' result <- send(client, batch)
#'
#' # Batch of template emails
#' recipients <- c("alice@example.com", "bob@example.com", "charlie@example.com")
#' templates <- lapply(recipients, function(email) {
#'   Template(
#'     from = "notifications@example.com",
#'     to = email,
#'     id = 12345678L,
#'     template_model = list(
#'       name = strsplit(email, "@")[[1]][1],  # Extract name from email
#'       link = sprintf("https://app.example.com/user/%s", email)
#'     )
#'   )
#' })
#' batch <- Batch(messages = templates)
#' send(client, batch)
#' }
#'
#' @seealso
#' \itemize{
#'   \item \url{https://postmarkapp.com/developer/api/email-api#send-batch-emails}
#'     for Email batch API
#'   \item \url{https://postmarkapp.com/developer/api/templates-api#send-batch-with-templates}
#'     for Template batch API
#'   \item [Email()] for single email messages
#'   \item [Template()] for template-based messages
#'   \item [send()] for sending batches
#' }
#'
#' @export
Batch <- new_class(
  "Batch",
  properties = list(
    messages = new_property(
      class = class_list,
      validator = function(value) {
        if (!length(value)) {
          pstmrk_abort(
            "`messages` must contain at least one message",
            class = "postmarkr_error_batch_empty"
          )
        }
        classes <- chr_ply(
          value,
          function(x) {
            cls <- class(x)
            if (length(cls)) cls[[1L]] else NA_character_
          }
        )

        valid_classes <- classes %in%
          paste0("postmarkr::", c("Email", "Template"))
        if (!all(valid_classes)) {
          invalid_positions <- which(!valid_classes)
          invalid_types <- unique(classes[!valid_classes])

          pstmrk_abort(
            sprintf(
              paste(
                "`messages` must only contain Email or Template objects.",
                "Found invalid type(s): %s at position(s): %s"
              ),
              paste(invalid_types, collapse = ", "),
              paste(invalid_positions, collapse = ", ")
            ),
            class = "postmarkr_error_batch_invalid_message_type"
          )
        }

        unique_classes <- unique(classes)
        if (length(unique_classes) > 1) {
          pstmrk_abort(
            sprintf(
              paste(
                "`messages` must all be the same type.",
                "Found: %s.",
                "Create separate batches for Email and Template objects."
              ),
              paste(unique_classes, collapse = ", ")
            ),
            class = "postmarkr_error_batch_mixed_types"
          )
        }
      }
    ),
    chunk_size = new_property(
      class = class_numeric,
      default = POSTMARK_MAX_BATCH_SIZE,
      validator = function(value) {
        if (!is_scalar_integerish(value) || value <= 0) {
          pstmrk_abort(
            "`chunk_size` must be a single positive integer",
            class = "postmarkr_error_batch_invalid_chunk_size"
          )
        }

        if (value > POSTMARK_MAX_BATCH_SIZE) {
          pstmrk_abort(
            sprintf(
              "`chunk_size` cannot exceed Postmark's limit of %d (got %d)",
              POSTMARK_MAX_BATCH_SIZE,
              value
            ),
            class = "postmarkr_error_batch_chunk_size_too_large"
          )
        }
      }
    )
  )
)

#' Get Number of Messages in Batch
#'
#' Returns the total number of messages in the batch.
#'
#' @param batch Batch object
#' @return Integer number of messages
#'
#' @noRd
#' @keywords internal
batch_size <- function(batch) {
   if (!is_batch(batch)) {
    pstmrk_abort(
      "`batch` must be a Batch object", 
      class = "postmarkr_error_not_batch_object"
    )
  }
  length(batch@messages)
}

#' Get Number of Chunks in Batch
#'
#' Calculates how many API calls will be needed to send the batch based on
#' the chunk size. Each chunk is sent as a separate API request.
#'
#' @param batch Batch object
#' @return Integer number of chunks
#'
#' @examples
#' \dontrun{
#' # 1000 messages with default chunk_size of 500
#' batch <- Batch(messages = emails)
#' batch_chunk_count(batch)  # Returns: 2
#'
#' # Same 1000 messages with custom chunk_size of 100
#' batch <- Batch(messages = emails, chunk_size = 100)
#' batch_chunk_count(batch)  # Returns: 10
#' }
#'
#' @noRd
#' @keywords internal
batch_chunk_count <- function(batch) {
   if (!is_batch(batch)) {
    pstmrk_abort(
      "`batch` must be a Batch object", 
      class = "postmarkr_error_not_batch_object"
    )
  }
  ceiling(length(batch@messages) / batch@chunk_size)
}

#' Get Message Type in Batch
#'
#' Returns the class of messages contained in the batch. All messages in a
#' batch are guaranteed to be the same type due to homogeneity validation.
#'
#' @param batch Batch object
#'
#' @return Character scalar: `postmarkr::Email` or `postmarkr::Template`
#'
#' @examples
#' \dontrun{
#' email_batch <- Batch(messages = list(Email(...), Email(...)))
#' batch_message_type(email_batch)  # Returns: "Email"
#'
#' template_batch <- Batch(messages = list(Template(...), Template(...)))
#' batch_message_type(template_batch)  # Returns: "Template"
#' }
#'
#' @noRd
#' @keywords internal
batch_message_type <- function(batch) {
   if (!is_batch(batch)) {
    pstmrk_abort(
      "`batch` must be a Batch object", 
      class = "postmarkr_error_not_batch_object"
    )
  }

  if (!length(batch@messages)) {
    pstmrk_abort(
      "Cannot get type from Batch object",
      class = "postmarkr_error_non_existent_message_type"
    )
  }

  class(batch@messages[[1L]])[[1L]]
}

#' Split Batch into Chunks
#'
#' Divides the batch messages into chunks suitable for sending via Postmark's
#' batch API. Each chunk contains at most `chunk_size` messages.
#'
#' @param batch Batch object
#' @return List of lists, where each inner list contains up to `chunk_size`
#'   messages. The last chunk may contain fewer messages.
#'
#' @examples
#' \dontrun{
#' recipients <- paste0("user", 1:1000, "@nutrivetpet.com")
#'
#' messages <- lapply(recipients, function(email) {
#'   Template(
#'     from = "sender@example.com",
#'     to = email,
#'     id = 12345L,
#'     template_model = list(name = email)
#'   )
#' })
#'
#' # Create batch with 1000 messages
#' batch <- Batch(messages = messages, chunk_size = 500)
#'
#' # Get chunks
#' chunks <- batch_get_chunks(batch)
#' length(chunks)  # Returns: 2
#' length(chunks[[1]])  # Returns: 500
#' length(chunks[[2]])  # Returns: 500
#'
#' # With non-divisible size
#' batch <- Batch(messages = messages[1:750], chunk_size = 500)
#' chunks <- batch_get_chunks(batch)
#' length(chunks)  # Returns: 2
#' length(chunks[[1]])  # Returns: 500
#' length(chunks[[2]])  # Returns: 250
#' }
#'
#' @noRd
#' @keywords internal
batch_get_chunks <- function(batch) {
  if (!is_batch(batch)) {
    pstmrk_abort(
      "`batch` must be a Batch object", 
      class = "postmarkr_error_not_batch_object"
    )
  }

  messages <- batch@messages
  n <- length(messages)
  chunk_size <- batch@chunk_size

  chunk_indices <- ceiling(seq_len(n) / chunk_size)

  unname(split(messages, chunk_indices))
}

is_batch <- function(x) {
  inherits(x, "postmarkr::Batch")
}
