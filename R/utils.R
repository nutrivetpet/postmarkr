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
      class = "missing_token"
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
    "statu",
    "todate",
    "fromdate",
    "subject",
    "messagestream",
    "metadata_",
    "type"
  )
}

capitalize_first <- function(x) {
  # TODO: vectorized
  stopifnot("`x` must be a single character string" = is_scalar_character(x))

  vec <- unlist(strsplit(x, split = ""))
  frst <- toupper(vec[[1L]])

  out <- paste0(
    frst,
    paste0(vec[-1], collapse = ""),
    collapse = ""
  )

  out
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
