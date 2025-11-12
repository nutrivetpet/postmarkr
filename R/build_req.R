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
        class = "not_supported_args"
      )
    }

    req <- req_url_query(req, !!!dots)
  }

  req
}
