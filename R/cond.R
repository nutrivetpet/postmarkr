pstmrk_abort <- function(
  message,
  ...,
  class = character(),
  call = caller_env()
) {
  abort(
    message = message,
    ...,
    class = c(class, "postmarkr_error"),
    call = call
  )
}

pstmrk_abort_api_change <- function(
  context,
  additional_info = NULL,
  call = caller_env()
) {
  msg <- c(
    context,
    "i" = "This might indicate a change in the Postmark API.",
    "i" = "Please report this issue at: https://github.com/nutrivetpet/postmarkr/issues"
  )

  if (!is.null(additional_info)) {
    msg <- c(msg, "x" = additional_info)
  }

  pstmrk_abort(
    msg,
    class = "postmarkr_error_stats_response_validation",
    call = call
  )
}

pstmrk_abort_invalid_scalar_character <- function(
  param_name,
  call = caller_env()
) {
  pstmrk_abort(
    sprintf(
      "`%s` must be a single non-empty character string if provided",
      param_name
    ),
    class = sprintf("postmarkr_error_invalid_%s", param_name),
    call = call
  )
}

pstmrk_abort_invalid_date_format <- function(
  param_name,
  example = "2024-01-01",
  call = caller_env()
) {
  pstmrk_abort(
    sprintf(
      "`%s` must be in YYYY-MM-DD format (e.g., '%s') if provided",
      param_name,
      example
    ),
    class = sprintf("postmarkr_error_invalid_%s", param_name),
    call = call
  )
}

pstmrk_abort_email_body_conflict <- function(call = caller_env()) {
  pstmrk_abort(
    "Cannot provide both `html_body` and `text_body` simultaneously",
    class = "postmarkr_error_email_body_conflict",
    call = call
  )
}

pstmrk_abort_email_missing_body <- function(call = caller_env()) {
  pstmrk_abort(
    "Must provide either `html_body` or `text_body`",
    class = "postmarkr_error_email_missing_body",
    call = call
  )
}

pstmrk_abort_email_too_many_recipients <- function(
  total,
  max = POSTMARK_MAX_RECIPIENTS_SINGLE,
  call = caller_env()
) {
  pstmrk_abort(
    sprintf(
      "Total recipients (To, Cc, Bcc combined) must not exceed %d (got %d)",
      max,
      total
    ),
    class = "postmarkr_error_email_too_many_recipients",
    call = call
  )
}

pstmrk_abort_email_invalid_track_links <- function(
  value,
  valid_options = c("None", "HtmlAndText", "HtmlOnly", "TextOnly"),
  call = caller_env()
) {
  pstmrk_abort(
    sprintf(
      "`track_links` must be one of: %s (got '%s')",
      paste(valid_options, collapse = ", "),
      value
    ),
    class = "postmarkr_error_email_invalid_track_links",
    call = call
  )
}

pstmrk_abort_template_invalid_tag <- function(call = caller_env()) {
  pstmrk_abort(
    "`tag` must be a single non-empty character string with 1000 characters or fewer",
    class = "postmarkr_error_invalid_tag",
    call = call
  )
}

pstmrk_abort_template_invalid_id <- function(call = caller_env()) {
  pstmrk_abort(
    "`id` must be a single positive integer",
    class = "postmarkr_error_invalid_template_id",
    call = call
  )
}

pstmrk_abort_template_too_many_recipients <- function(
  total,
  max = POSTMARK_MAX_RECIPIENTS_SINGLE,
  call = caller_env()
) {
  pstmrk_abort(
    sprintf(
      "Total recipients (To, Cc, Bcc combined) must not exceed %d (got %d)",
      max,
      total
    ),
    class = "postmarkr_error_template_too_many_recipients",
    call = call
  )
}

pstmrk_abort_template_params_invalid_count <- function(call = caller_env()) {
  pstmrk_abort(
    "`count` must be a single positive integer",
    class = "postmarkr_error_invalid_count",
    call = call
  )
}

pstmrk_abort_invalid_email <- function(
  email,
  arg_name = "email",
  call = caller_env()
) {
  pstmrk_abort(
    sprintf("Invalid email format in `%s`: %s", arg_name, email),
    class = "postmarkr_error_invalid_email",
    call = call
  )
}

pstmrk_abort_batch_empty <- function(call = caller_env()) {
  pstmrk_abort(
    "`messages` must contain at least one message",
    class = "postmarkr_error_batch_empty",
    call = call
  )
}

pstmrk_abort_batch_invalid_message_type <- function(
  invalid_types,
  positions,
  call = caller_env()
) {
  pstmrk_abort(
    sprintf(
      paste(
        "`messages` must only contain Email or Template objects.",
        "Found invalid type(s): %s at position(s): %s"
      ),
      paste(invalid_types, collapse = ", "),
      paste(positions, collapse = ", ")
    ),
    class = "postmarkr_error_batch_invalid_message_type",
    call = call
  )
}

pstmrk_abort_batch_mixed_types <- function(
  types,
  call = caller_env()
) {
  pstmrk_abort(
    sprintf(
      paste(
        "`messages` must all be the same type.",
        "Found: %s.",
        "Create separate batches for Email and Template objects."
      ),
      paste(types, collapse = " and ")
    ),
    class = "postmarkr_error_batch_mixed_types",
    call = call
  )
}

pstmrk_abort_batch_invalid_chunk_size <- function(call = caller_env()) {
  pstmrk_abort(
    "`chunk_size` must be a single positive integer",
    class = "postmarkr_error_batch_invalid_chunk_size",
    call = call
  )
}

pstmrk_abort_batch_chunk_size_too_large <- function(
  value,
  max = POSTMARK_MAX_BATCH_SIZE,
  call = caller_env()
) {
  pstmrk_abort(
    sprintf(
      "`chunk_size` cannot exceed Postmark's limit of %d (got %d)",
      max,
      value
    ),
    class = "postmarkr_error_batch_chunk_size_too_large",
    call = call
  )
}
