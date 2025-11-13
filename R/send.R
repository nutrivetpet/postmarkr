#' Send
#' @export
send <- new_generic("send", c("client", "message"))

method(send, list(postmarkr, email)) <- function(client, message) {
  req <- build_req_s7(
    client = client,
    endpoint = "/email",
    method = "POST"
  )

  bdy <- as_api_body(message)
  bdy$MessageStream <- client@message_stream

  req <- req_body_json(req, bdy)

  resp <- req_perform(req)

  postmarkr_response(
    data = resp_body_json(resp, simplifyVector = TRUE),
    status = resp_status(resp),
    request = req,
    response = resp,
    success = isFALSE(resp_is_error(resp))
  )
}
