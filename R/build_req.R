build_req <- function(endpoint, method, token = NULL, ...) {
  dots <- rlang::list2(...)

  if (is.null(token)) {
    token <- get_token()
  }

  req <-
    httr2::request("https://api.postmarkapp.com") |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_method(method) |>
    build_header(token)

  if (length(dots)) {
    args <- rlang::names2(dots)

    if (!all(args %in% supported_args())) {
      rlang::abort(
        "Some arguments are not supported.",
        class = "not_supported_args"
      )
    }

    req <- httr2::req_url_query(req, !!!dots)

    return(req)
  }

  req
}
