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

# Email validation errors

email_abort_body_conflict <- function(call = caller_env()) {
  pstmrk_abort(
    "Cannot provide both `html_body` and `text_body` simultaneously",
    class = "postmarkr_email_body_conflict",
    call = call
  )
}

email_abort_missing_body <- function(call = caller_env()) {
  pstmrk_abort(
    "Must provide either `html_body` or `text_body`",
    class = "postmarkr_email_missing_body",
    call = call
  )
}

email_abort_too_many_recipients <- function(
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
    class = "postmarkr_email_too_many_recipients",
    call = call
  )
}

email_abort_invalid_track_links <- function(
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
    class = "postmarkr_email_invalid_track_links",
    call = call
  )
}
