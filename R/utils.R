get_token <- function() {
  token <- Sys.getenv("POSTMARK_SERVER_TOKEN")
  if (!nzchar(token)) {
    rlang::abort(
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
  httr2::req_headers(
    req,
    "Accept" = "application/json",
    "X-Postmark-Server-Token" = token
  )
}

generate_offset_batches <- function(n) {
  stopifnot(rlang::is_integer(n, 1L))
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
  stopifnot(rlang::is_character(x, n = 1L))

  vec <- unlist(strsplit(x, split = ""))
  frst <- toupper(vec[[1L]])

  out <- paste0(
    frst,
    paste0(vec[-1], collapse = ""),
    collapse = ""
  )

  out

}
