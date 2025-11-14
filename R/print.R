#' @export
method(print, Postmarkr) <- function(x, ...) {
  redacted_token <- paste0(
    substr(x@token, 1, 8),
    "-****-****-****-************"
  )

  cat("<Postmarkr::postmark>\n")
  cat("  @ token          : chr ", redacted_token, "\n", sep = "")
  cat("  @ message_stream : chr ", x@message_stream, "\n", sep = "")
  cat("  @ base_url       : chr ", x@base_url, "\n", sep = "")
  cat("  @ timeout        : num ", x@timeout, " seconds\n", sep = "")
  invisible(x)
}
