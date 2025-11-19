supported_args <- function() {
  c(
    "count",
    "offset",
    "recipient",
    "fromemail",
    "tag",
    "status",
    "todate",
    "fromdate",
    "subject",
    "messagestream",
    "metadata_",
    "type"
  )
}

collapse_comma <- function(x) {
  stopifnot("`x` must be a character vector" = is_character(x))
  paste0(x, collapse = ", ")
}

#' Extract email address from name-formatted email strings
#'
#' @param x Character vector of email addresses (may include names)
#' @return Character vector of plain email addresses
#' @keywords internal
#' @noRd
extract_email <- function(x) {
  stopifnot("`x` must be a character vector" = is_character(x))

  pattern <- "<([^>]+)>"

  chr_ply(x, function(email) {
    match <- regmatches(email, regexec(pattern, email))[[1L]]
    if (length(match) > 1L) {
      match[[2L]]
    } else {
      email
    }
  })
}

#' Validate email address format
#'
#' @param email Character vector of email addresses to validate
#' @param arg_name Name of the argument being validated (for error messages)
#' @param call Calling environment for error reporting
#' @return NULL invisibly (throws error if invalid)
#' @keywords internal
#' @noRd
validate_email <- function(email, arg_name = "email", call = caller_env()) {
  stopifnot("`email` must be a character vector" = is_character(email))

  plain_emails <- extract_email(email)

  email_regex <- "^[\\w._%+-]+@[\\w.-]+\\.[A-Za-z]{2,}$"

  valid <- grepl(email_regex, plain_emails, perl = TRUE)

  if (!all(valid)) {
    invalid_emails <- email[!valid]
    pstmrk_abort_invalid_email(
      invalid_emails[1L],
      arg_name = arg_name,
      call = call
    )
  }

  invisible(NULL)
}
