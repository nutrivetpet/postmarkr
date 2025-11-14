#' Print a Postmarkr Client Object
#'
#' @description
#' Pretty-prints a `Postmarkr` client object, displaying all properties with
#' their types and values. Sensitive information (API tokens) is automatically
#' redacted for security.
#'
#' @param x A `Postmarkr` object to print
#' @param ... Additional arguments (currently unused, included for S3 generic
#'   compatibility)
#'
#' @return Invisibly returns the input object `x`, allowing for piping and
#'   chaining operations
#'
#' @examples
#' \dontrun{
#' # Create and print a Postmarkr client
#' client <- Postmarkr(
#'   token = "your-server-token",
#'   message_stream = "outbound"
#' )
#' print(client)
#' }
#' 
#' @export
method(print, Postmarkr) <- function(x, ...) {
  cat("<postmarkr::Postmarkr>\n")

  props <- prop_names(x)

  for (prop in props) {
    value <- prop(x, prop)
    type <- class(value)[1]

    if (prop == "token") {
      redacted <- paste0(substr(value, 1, 8), "-****-****-****-************")
      cat(sprintf(" @ %-15s: <%s> %s\n", prop, type, redacted))
    } else {
      cat(sprintf(" @ %-15s: <%s> %s\n", prop, type, format(value)))
    }
  }

  invisible(x)
}
