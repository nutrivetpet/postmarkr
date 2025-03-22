#' List Postmark Templates
#'
#' Retrieves a list of templates from the Postmark API. Templates
#' can be filtered by type and paginated using count and offset parameters.
#'
#' @inheritParams outbound_messages_fetch
#' @param count An integer specifying the number of templates to retrieve.
#' @param type A string specifying the template type to filter by: "all",
#'   "standard", or "layout". Defaults to "all".
#'
#' @return A data frame (or tibble if tibble is installed) containing the
#'   templates information. The returned data includes template details from the
#'   Postmark API.
#'
#' @examples
#' \dontrun{
#' # Get the first 10 templates
#' templates <- template_list(count = 10)
#'
#' # Get only layout templates
#' layouts <- template_list(count = 50, type = "layout")
#'
#' # Use a specific token
#' templates <- template_list(count = 10, token = "your-api-token")
#' }
#' @export
template_list <- function(count, type = "all", token = NULL) {

  stopifnot(
    rlang::is_scalar_integer(count),
    count > 0
  )

  typ <- rlang::arg_match(type, c("all", "standard", "layout"))

  req <- build_req(
    "templates",
    "GET",
    token = token,
    count = count,
    offset = 0L,
    type = capitilize_first(type)
  )

  resp <- httr2::req_perform(req)
  out <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  dat <- out[["Templates"]]

  if (rlang::is_installed("tibble")) {
    dat <- tibble::as_tibble(dat)
  }

  dat

}
