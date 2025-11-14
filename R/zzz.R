.onLoad <- function(...) {
  S7::methods_register()
}

method(as_api_body, Email) <- function(x) {
  body <- list(
    From = x@from,
    To = paste0(x@to, collapse = ", ")
  )

  if (length(x@subject)) {
    body$Subject <- x@subject
  }

  if (length(x@cc)) {
    body$Cc <- paste0(x@cc, collapse = ", ")
  }

  if (length(x@bcc)) {
    body$Bcc <- paste0(x@bcc, collapse = ", ")
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
