#' Send an Email Using a Template
#'
#' Sends an email using a predefined template.
#'
#' @inheritParams email_send_single
#' @param id A single integer. The template ID in Postmark.
#' @param template_model A named list. Variables to be populated in the
#'   template.
#' @param tag A single character string. Optional tag for categorizing the
#'   email. Maximum 1000 characters. Default is NULL.
#' @param track_opens A logical value. Whether to track when recipients open the
#'   email. Default is FALSE.
#'
#' @return A data frame containing the JSON response from the Postmark API,
#'   invisibly.
#'
#' @examples
#' \dontrun{
#' template_send_email(
#'   from = "sender@example.com",
#'   to = c("recipient1@example.com", "recipient2@example.com"),
#'   id = 12345,
#'   template_model = list(
#'     name = "John",
#'     message = "Hello from Postmark!"
#'   ),
#'   msg_stream = "outbound",
#'   tag = "welcome-email",
#'   track_opens = TRUE
#' )
#' }
#'
#' @export
template_send_email <- function(
    from,
    to,
    id,
    template_model,
    msg_stream,
    tag = NULL,
    track_opens = FALSE,
    token = NULL
) {

  stopifnot(
    rlang::is_scalar_character(from),
    rlang::is_character(to),
    length(to) <= 50L,
    rlang::is_scalar_integer(id),
    rlang::is_list(template_model),
    rlang::is_named(template_model),
    nchar(tag) <= 1e3L,
    rlang::is_scalar_logical(track_opens)
  )

  if (!is.null(tag)) {
    stopifnot(rlang::is_scalar_character(tag), nchar(tag) <= 1e3L)
  }

  msg_stream <- rlang::arg_match(msg_stream, c("outbound", "broadcast"))

  to <- paste0(to, collapse = ", ")

  bdy <- list(
    From = from,
    To = to,
    Tag = tag,
    TemplateId = id,
    TemplateModel = as.list(template_model),
    MessageStream = msg_stream,
    TrackOpens = track_opens
  )

  req <-
    build_req("/email/withTemplate/", "POST", token) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_body_json(bdy)

  resp <- httr2::req_perform(req)

  if (httr2::resp_is_error(resp)) {
    httr2::resp_check_status(resp)
  }

  dat <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (rlang::is_installed("tibble")) {
    dat <- tibble::as_tibble(dat)
  }

  invisible(dat)

}

#' Send Batch Email Using a Template
#'
#' Send email to more than 500 recipients, sending multiple POST requests via
#' [httr2::req_perform_sequential()].
#'
#' @inheritParams template_send_email
#'
#' @return A data frame containing the JSON response from the Postmark API,
#'   invisibly.
#'
#' @export
template_send_email_bulk <- function(
    from,
    to,
    id,
    template_model,
    msg_stream,
    tag = NULL,
    track_opens = FALSE,
    token = NULL
) {

  stopifnot(
    rlang::is_scalar_character(from),
    rlang::is_character(to),
    rlang::is_scalar_integer(id),
    rlang::is_list(template_model),
    rlang::is_named(template_model),
    rlang::is_scalar_logical(track_opens)
  )

  if (!is.null(tag)) {
    stopifnot(rlang::is_scalar_character(tag), nchar(tag) <= 1e3L)
  }

  msg_stream <- rlang::arg_match(msg_stream, c("outbound", "broadcast"))

  template_model <- rep_list(template_model, length(to))

  bdy <- Map(
    function(from, to, id, template_model, track_opens, msg_stream) {
      list(
        From = from,
        To = to,
        TemplateId = id,
        TemplateModel = template_model,
        TrackOpens = track_opens,
        MessageStream = msg_stream
      )
    },
    from,
    to,
    id,
    template_model,
    track_opens,
    msg_stream,
    USE.NAMES = FALSE
  )

  if (!is.null(tag)) {
    bdy <- Map(
      function(x, y) {
        x[["Tag"]] <- y
        x
      },
      bdy,
      tag
    )
  }

  bdy_lst <- unname(split(bdy, (seq_along(bdy) - 1) %/% 500))

  bdy <- lapply(bdy_lst, \(x) list("Messages" = x))

  req_lst <- lapply(
    bdy,
    function(x) {
      build_req("/email/batchWithTemplates/", "POST", token) |>
        httr2::req_headers("Content-Type" = "application/json") |>
        httr2::req_body_json(x)
    }
  )

  resp <- httr2::req_perform_sequential(
    req_lst,
    on_error = "continue",
    progress = TRUE
  )

  dat_lst <- lapply(resp, \(x) httr2::resp_body_json(x, simplifyVector = TRUE))
  dat <- Reduce(rbind, dat_lst)

  if (rlang::is_installed("tibble")) {
    dat <- tibble::as_tibble(dat)
  }

  invisible(dat)

}

#' List Templates
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
