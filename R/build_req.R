build_req <- function(endpoint, method, env, ...) {
  dots <- list2(...)
  token <- get_token(env)

  req <-
    request(POSTMARK_BASE_URL) |>
    req_url_path_append(endpoint) |>
    req_method(method) |>
    build_header(token)

  if (length(dots)) {
    args <- names2(dots)

    if (!all(args %in% supported_args())) {
      abort(
        "Some arguments are not supported.",
        class = "postmarkr_error_not_supported_args"
      )
    }

    req <- req_url_query(req, !!!dots)
  }

  req
}

#' Build HTTP Request for S7 Client
#'
#' Creates an httr2 request object using a postmarkr S7 client.
#'
#' @param client A client object as returned by [Postmarkr()].
#' @param endpoint The API endpoint path (e.g., "/stats/outbound").
#' @param method HTTP method (e.g., "GET", "POST").
#' @param ... Additional query parameters.
#'
#' @return An httr2 request object.
#' @noRd
#' @keywords internal
build_req_S7 <- function(client, endpoint, method, ...) {
  dots <- list2(...)

  req <-
    request(client@base_url) |>
    req_url_path_append(endpoint) |>
    req_method(method) |>
    req_headers(
      "Accept" = "application/json",
      "X-Postmark-Server-Token" = client@token,
      .redact = "X-Postmark-Server-Token"
    )

  if (!is.null(client@timeout)) {
    req <- req_timeout(req, client@timeout)
  }

  if (client@verbose) {
    req <- req_verbose(req)
  }

  if (length(dots)) {
    args <- names2(dots)

    if (!all(args %in% supported_args())) {
      pstmrk_abort(
        "Some arguments are not supported.",
        class = "postmarkr_error_not_supported_args"
      )
    }

    req <- req_url_query(req, !!!dots)
  }

  req
}
