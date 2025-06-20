#' Send a single email
#'
#' This function sends a single email via the Postmark API service. It supports
#' both HTML and plain text email formats (but not both simultaneously) and can
#' be used for both the "outbound" and "broadcast") email message streams.
#'
#' @param from Character scalar. Email address of the sender.
#' @param to Character vector. Email addresses of recipients (max 50).
#' @param msg_stream Character scalar. Either "outbound" or "broadcast".
#' @param subject Character scalar. Email subject line.
#' @param html_body Character scalar. HTML content of the email.
#' @param text_body Character scalar. Plain text content of the email.
#' @inheritParams outbound_messages_fetch
#'
#' @return A data frame or tibble (if tibble is installed) containing the
#'   response details.
#'
#' @rdname email
#'
#' @examples
#' \dontrun{
#'  email_send_single(
#'   from = "sender@example.com",
#'   to = "recipient@example.com",
#'   msg_stream = "outbound",
#'   subject = "Hello from R",
#'   html_body = "<h1>Hello world!</h1>",
#' )
#' }
#' @export
email_send_single <- function(
  from,
  to,
  msg_stream,
  subject = NULL,
  html_body = NULL,
  text_body = NULL,
  token = NULL
) {
  stopifnot(
    is_scalar_character(from),
    is_character(to),
    length(to) <= 50L
  )

  msg_stream <- arg_match(msg_stream, c("outbound", "broadcast"))

  if (!is.null(subject)) {
    stopifnot(is_scalar_character(subject))
  }

  has_html <- !is.null(html_body)
  has_text <- !is.null(text_body)

  if (has_html) {
    stopifnot(is_scalar_character(html_body))
  }

  if (has_text) {
    stopifnot(is_scalar_character(text_body))
  }

  if (has_html && has_text) {
    abort(
      "Cannot provide both `html_body` and `text_body`.",
      class = "too_many_args"
    )
  }

  if (!has_html && !has_text) {
    abort(
      "Must provide either `html_body` or `text_body`.",
      class = "missing_args"
    )
  }

  to <- paste0(to, collapse = ", ")

  bdy <- list(
    From = from,
    To = to,
    Subject = subject,
    MessageStream = msg_stream
  )

  if (is.null(text_body)) {
    bdy[["HtmlBody"]] <- html_body
  } else {
    bdy[["TextBody"]] <- text_body
  }

  req <-
    build_req("email", "POST", token) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(bdy)

  resp <- req_perform(req)

  if (resp_is_error(resp)) {
    resp_check_status(resp)
  }

  dat <- resp_body_json(resp, simplifyVector = TRUE)

  if (is_installed("tibble")) {
    dat <- tibble::as_tibble(dat)
  }

  dat
}
