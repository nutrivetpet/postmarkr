.onLoad <- function(...) {
  S7::methods_register()
}

method(as_api_body, Email) <- function(x) {
  body <- list(
    From = x@from,
    To = paste0(x@to, collapse = ", ")
  )

  if (length(x@subject) > 0) {
    body$Subject <- x@subject
  }

  if (length(x@cc) > 0) {
    body$Cc <- paste0(x@cc, collapse = ", ")
  }

  if (length(x@bcc) > 0) {
    body$Bcc <- paste0(x@bcc, collapse = ", ")
  }

  if (length(x@reply_to) > 0) {
    body$ReplyTo <- x@reply_to
  }

  if (length(x@tag) > 0) {
    body$Tag <- x@tag
  }

  if (length(x@html_body) > 0) {
    body$HtmlBody <- as.character(x@html_body)
  } else if (length(x@text_body) > 0) {
    body$TextBody <- as.character(x@text_body)
  } else {
    pstmrk_abort("Missing body", class = "postmarkr_error_internal")
  }

  if (length(x@track_opens) > 0) {
    body$TrackOpens <- x@track_opens
  }

  if (length(x@track_links) > 0) {
    body$TrackLinks <- x@track_links
  }

  if (length(x@metadata) > 0) {
    body$Metadata <- x@metadata
  }

  if (length(x@headers) > 0) {
    body$Headers <- x@headers
  }

  if (length(x@attachments) > 0) {
    body$Attachments <- x@attachments
  }

  body
}
