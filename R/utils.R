get_token <- function() {
  token <- Sys.getenv("POSTMARK_SERVER_TOKEN")
  if (!nzchar(token)) {
    abort(
      c(
        "Cannot find token for API authentication.",
        i = "Did you forget to set the `POSTMARK_SERVER_TOKEN` env. variable?"
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
  stopifnot(is_integer(n, 1L))
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

capitilize_first <- function(x) {
  # TODO: vectorized
  stopifnot(is_character(x, n = 1L))

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
    # is_character(x),
    is_scalar_integer(n),
    n > 0L,
    length(x) > 0L
  )

  groups <- ceiling(seq_along(x) / n)

  unname(split(x, groups))

}

rep_list <- function(x, n) {

  stopifnot(is_list(x), is_named(x))

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
