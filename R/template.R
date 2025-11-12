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
#' @return A data frame or tibble (if tibble is installed) containing the
#'   response details, invisibly.
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
  track_opens = FALSE
) {
  template_send_email_impl(
    from = from,
    to = to,
    id = id,
    template_model = template_model,
    msg_stream = msg_stream,
    tag = tag,
    track_opens = track_opens,
    env = "live"
  )
}

template_send_email_impl <- function(
  from,
  to,
  id,
  template_model,
  msg_stream,
  tag = NULL,
  track_opens = FALSE,
  env = c("live", "test")
) {
  recipients_error <- sprintf(
    "`to` must have %d or fewer recipients",
    POSTMARK_MAX_RECIPIENTS_SINGLE
  )
  stopifnot(
    "`from` must be a single character string" = is_scalar_character(from),
    "`to` must be a character vector" = is_character(to),
    recipients_error = length(to) <= POSTMARK_MAX_RECIPIENTS_SINGLE,
    "`id` must be a single integer" = is_scalar_integer(id),
    "`template_model` must be a list" = is_list(template_model),
    "`template_model` must be a named list" = is_named(template_model),
    "`tag` must be 1000 characters or fewer" = nchar(tag) <= 1e3L,
    "`track_opens` must be a single logical value" = is_scalar_logical(
      track_opens
    )
  )

  if (!is.null(tag)) {
    stopifnot(
      "`tag` must be a single character string" = is_scalar_character(tag),
      "`tag` must be 1000 characters or fewer" = nchar(tag) <= 1e3L
    )
  }

  msg_stream <- arg_match(msg_stream, c("outbound", "broadcast"))

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
    build_req("/email/withTemplate/", "POST", env) |>
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

  invisible(dat)
}

#' Send Batch Email Using a Template
#'
#' Send email to more than 500 recipients, sending multiple POST requests via
#' `req_perform_sequential()`.
#'
#' @inheritParams template_send_email
#'
#' @return A data frame or tibble (if tibble is installed) containing the
#'   response details, invisibly.
#'
#' @export
template_send_email_batch <- function(
  from,
  to,
  id,
  template_model,
  msg_stream,
  tag = NULL,
  track_opens = FALSE
) {
  template_send_email_batch_impl(
    from = from,
    to = to,
    id = id,
    template_model = template_model,
    msg_stream = msg_stream,
    tag = tag,
    track_opens = track_opens,
    env = "live"
  )
}

template_send_email_batch_impl <- function(
  from,
  to,
  id,
  template_model,
  msg_stream,
  tag = NULL,
  track_opens = FALSE,
  env = c("live", "test")
) {
  stopifnot(
    "`from` must be a single character string" = is_scalar_character(from),
    "`to` must be a character vector" = is_character(to),
    "`id` must be a single integer" = is_scalar_integer(id),
    "`template_model` must be a list" = is_list(template_model),
    "`template_model` must be a named list" = is_named(template_model),
    "`track_opens` must be a single logical value" = is_scalar_logical(
      track_opens
    )
  )

  if (!is.null(tag)) {
    stopifnot(
      "`tag` must be a single character string" = is_scalar_character(tag),
      "`tag` must be 1000 characters or fewer" = nchar(tag) <= 1e3L
    )
  }

  msg_stream <- arg_match(msg_stream, c("outbound", "broadcast"))

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

  bdy_lst <- unname(split(bdy, (seq_along(bdy) - 1) %/% POSTMARK_MAX_BATCH_SIZE))

  bdy <- lapply(bdy_lst, \(x) list("Messages" = x))

  req_lst <- lapply(
    bdy,
    function(x) {
      build_req("/email/batchWithTemplates/", "POST", env) |>
        req_headers("Content-Type" = "application/json") |>
        req_body_json(x)
    }
  )

  resp <- req_perform_sequential(
    req_lst,
    on_error = "continue",
    progress = TRUE
  )

  dat_lst <- lapply(resp, \(x) resp_body_json(x, simplifyVector = TRUE))
  dat <- dplyr::bind_rows(dat_lst)

  if (is_installed("tibble")) {
    dat <- tibble::as_tibble(dat)
  }

  invisible(dat)
}

#' List Templates
#'
#' Retrieves a list of templates from the Postmark API. Templates
#' can be filtered by type and paginated using count and offset parameters.
#'
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
#' # Get all templates with pagination
#' templates <- template_list(count = 100)
#' }
#' @export
template_list <- function(count, type = "all") {
  template_list_impl(count = count, type = type, env = "live")
}

template_list_impl <- function(count, type = "all", env = c("live", "test")) {
  stopifnot(
    "`count` must be a single integer" = is_scalar_integer(count),
    "`count` must be greater than 0" = count > 0
  )

  typ <- arg_match(type, c("all", "standard", "layout"))

  req <- build_req(
    "templates",
    "GET",
    env,
    count = count,
    offset = 0L,
    type = capitalize_first(type)
  )

  resp <- req_perform(req)
  out <- resp_body_json(resp, simplifyVector = TRUE)
  dat <- out[["Templates"]]

  if (is_installed("tibble")) {
    dat <- tibble::as_tibble(dat)
  }

  dat
}
