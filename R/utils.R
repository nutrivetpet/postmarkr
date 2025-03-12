get_token <- function() {
  Sys.getenv("POSTMARK_SERVER_TOKEN")
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
