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
    "metadata_"
  )
}
