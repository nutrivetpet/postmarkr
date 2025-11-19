#' @include email.R template.R

#' Convert Body Format
#' Convert S7 objects to Postmark API request body format
#' @noRd
#' @keywords internal
as_api_body <- new_generic("as_api_body", "x", function(x, ...) {
  S7_dispatch()
})

method(as_api_body, Email) <- function(x, ...) {
  body <- list(
    From = x@from,
    To = collapse_comma(x@to)
  )

  if (length(x@subject)) {
    body$Subject <- x@subject
  }

  if (length(x@cc)) {
    body$Cc <- collapse_comma(x@cc)
  }

  if (length(x@bcc)) {
    body$Bcc <- collapse_comma(x@bcc)
  }

  if (length(x@reply_to)) {
    body$ReplyTo <- x@reply_to
  }

  if (length(x@tag)) {
    body$Tag <- x@tag
  }

  if (length(x@html_body)) {
    body$HtmlBody <- as.character(x@html_body)
  } else if (length(x@text_body)) {
    body$TextBody <- as.character(x@text_body)
  } else {
    pstmrk_abort("Missing body", class = "postmarkr_error_internal")
  }

  if (length(x@track_opens)) {
    body$TrackOpens <- x@track_opens
  }

  if (length(x@track_links)) {
    body$TrackLinks <- x@track_links
  }

  if (length(x@metadata)) {
    body$Metadata <- x@metadata
  }

  if (length(x@headers)) {
    body$Headers <- x@headers
  }

  if (length(x@attachments)) {
    body$Attachments <- x@attachments
  }

  body
}

method(as_api_body, Template) <- function(x, ...) {
  body <- list(
    From = x@from,
    To = collapse_comma(x@to),
    TemplateModel = as.list(x@template_model)
  )

  if (length(x@id)) {
    body$TemplateId <- x@id
  } else {
    body$TemplateAlias <- x@alias
  }

  if (length(x@cc)) {
    body$Cc <- collapse_comma(x@cc)
  }

  if (length(x@bcc)) {
    body$Bcc <- collapse_comma(x@bcc)
  }

  if (length(x@inline_css)) {
    body$InlineCss <- x@inline_css
  }

  if (length(x@tag)) {
    body$Tag <- x@tag
  }

  if (length(x@reply_to)) {
    body$ReplyTo <- x@reply_to
  }

  if (length(x@headers)) {
    body$Headers <- x@headers
  }

  if (length(x@track_opens)) {
    body$TrackOpens <- x@track_opens
  }

  if (length(x@track_links)) {
    body$TrackLinks <- x@track_links
  }

  if (length(x@attachments)) {
    body$Attachments <- x@attachments
  }

  if (length(x@metadata)) {
    body$Metadata <- x@metadata
  }

  body
}
