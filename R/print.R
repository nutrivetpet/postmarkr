#' @export
method(print, Postmarkr) <- function(x, ...) {
  # Temporarily redact the token for printing
  original_token <- x@token
  x@token <- paste0(
    substr(original_token, 1, 8),
    "-****-****-****-************"
  )

  # Use default S7 print method with redacted token
  print.default(x)

  # Restore original token (just in case, though x is local)
  x@token <- original_token

  invisible(x)
}
