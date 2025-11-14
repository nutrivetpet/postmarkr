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

stats_abort_invalid_date_format <- function(
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
