get_token <- function(env = c("live", "test")) {
  env <- arg_match(env, env)
  envar <- switch(
    env,
    test = "POSTMARK_TEST_SERVER_TOKEN",
    live = "POSTMARK_SERVER_TOKEN"
  )
  token <- Sys.getenv(envar)
  if (!nzchar(token)) {
    abort(
      c(
        "Cannot find token for API authentication.",
        i = sprintf(
          "Did you forget to set the `%s` env. variable?",
          envar
        )
      ),
      class = "postmarkr_error_missing_token"
    )
  }
  token
}

build_header <- function(req, token) {
  req_headers(
    req,
    "Accept" = "application/json",
    "X-Postmark-Server-Token" = token,
    .redact = "X-Postmark-Server-Token"
  )
}

generate_offset_batches <- function(n) {
  stopifnot("`n` must be a single integer" = is_integer(n, 1L))
  offset <- seq(0L, n, by = 500L)
  count <- rep(500L, length(offset))
  data.frame(count = count, offset = offset)
}

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

capitalize_first <- function(x) {
  stopifnot(
    "`x` must be a character vector" = is_character(x)
  )

  vec <- strsplit(x, split = "")
  uppercase <- lapply(vec, function(x) {
    if (length(x) == 0L) "" else toupper(x[[1L]])
  })

  out <- Map(
    function(x, y) {
      replace(x, 1L, y)
    },
    vec,
    uppercase,
    USE.NAMES = FALSE
  )

  unlist(lapply(out, function(x) paste0(x, collapse = "")))
}

split_vec <- function(x, n) {
  stopifnot(
    # "`x` must be a character vector" = is_character(x),
    "`n` must be a single integer" = is_scalar_integer(n),
    "`n` must be greater than 0" = n > 0L,
    "`x` must not be empty" = length(x) > 0L
  )

  groups <- ceiling(seq_along(x) / n)

  unname(split(x, groups))
}

rep_list <- function(x, n) {
  stopifnot(
    "`x` must be a list" = is_list(x),
    "`x` must be a named list" = is_named(x)
  )

  template_fn <- function(...) {
    args <- list(...)
    args <- set_names(x, names(x))
    args
  }

  reps <- lapply(x, function(x) rep(x, n))

  args <- c(
    list(f = template_fn),
    reps,
    list(USE.NAMES = FALSE)
  )

  exec("Map", !!!args)
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
