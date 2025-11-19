#' @include batch.R email.R template.R
NULL

#' Send Email, Template and Batch Objects
#'
#' @description
#' Generic function to send emails through the Postmark API. This function
#' dispatches to the appropriate method based on the message type ([Email],
#' [Template], [Batch]).
#'
#' @param client A [Client] client object.
#' @param message An [Email], [Template] or [Batch] object to send.
#' @param ... Additional arguments passed to methods.
#'
#' @return A `Response` S7 object with the following properties:
#' \describe{
#'   \item{data}{List containing API response data including `MessageID`,
#'     `SubmittedAt`, `To`, `ErrorCode`, and `Message` fields}
#'   \item{status}{HTTP status code (200 for success)}
#'   \item{request}{The httr2 request object used for the API call}
#'   \item{response}{The httr2 response object from the API}
#'   \item{success}{Logical indicating if the email was sent successfully}
#' }
#'
#' @examples
#' \dontrun{
#' # Create client
#' client <- Client(
#'   token = "your-server-token",
#'   message_stream = "outbound"
#' )
#'
#' # Send a regular email
#' email <- email(
#'   from = "sender@example.com",
#'   to = "recipient@example.com",
#'   subject = "Hello from postmarkr",
#'   text_body = "This is a test email sent via R."
#' )
#'
#' response <- send(client, email)
#'
#' # Check if successful
#' if (response@success) {
#'   cat("Email sent successfully!\n")
#'   cat("Message ID:", response@data$MessageID, "\n")
#'   cat("Submitted at:", response@data$SubmittedAt, "\n")
#' }
#'
#' # Send HTML email with tracking
#' html_email <- email(
#'   from = "notifications@example.com",
#'   to = "user@example.com",
#'   subject = "Welcome to our service",
#'   html_body = "<h1>Welcome!</h1><p>Thanks for signing up.</p>",
#'   track_opens = TRUE,
#'   track_links = "HtmlOnly",
#'   tag = "welcome-email"
#' )
#'
#' response <- send(client, html_email)
#'
#' # Send a template email
#' template <- Template(
#'   from = "sender@example.com",
#'   to = "recipient@example.com",
#'   id = 12345678L,
#'   template_model = list(name = "John", order_id = "ORD-123")
#' )
#'
#' response <- send(client, template)
#' }
#'
#' @seealso
#' * [Client] for creating an API client
#' * [Email] for creating email objects
#' * [Template] for creating template objects
#' * \url{https://postmarkapp.com/developer/api/email-api#send-a-single-email}
#'   for Postmark email API documentation
#' * \url{https://postmarkapp.com/developer/api/templates-api#send-email-with-template}
#'   for Postmark template API documentation
#'
#' @export
send <- new_generic(
  "send",
  c("client", "message"),
  function(client, message, ...) {
    S7_dispatch()
  }
)

method(send, list(Client, Email)) <- function(client, message, ...) {
  send_message_individual(client, message, "/email")
}

method(send, list(Client, Template)) <- function(
  client,
  message,
  ...
) {
  send_message_individual(client, message, "/email/withTemplate")
}

send_message_individual <- function(client, message, endpoint) {
  req <- build_req_S7(
    client = client,
    endpoint = endpoint,
    method = "POST"
  )

  bdy <- as_api_body(message)
  bdy$MessageStream <- client@message_stream

  req <- req_body_json(req, bdy)

  resp <- req_perform(req)

  Response(
    data = resp_body_json(resp, simplifyVector = TRUE),
    status = resp_status(resp),
    request = req,
    response = resp,
    success = isFALSE(resp_is_error(resp))
  )
}

method(send, list(Client, Batch)) <- function(
  client,
  message,
  ...
) {
  typ <- tolower(batch_message_type(message))
  switch(
    typ,
    "postmarkr::email" = send_message_batch(
      client,
      message,
      "/email/batch"
    ),
    "postmarkr::template" = send_message_batch(
      client,
      message,
      "/email/batchWithTemplates"
    )
  )
}

send_message_batch <- function(client, message, endpoint) {
  chunks <- batch_get_chunks(message)

  bdy_lst <- lapply(
    chunks,
    function(ck) {
      out <- lapply(
        ck,
        function(msg) {
          api_body <- as_api_body(msg)
          api_body$MessageStream <- client@message_stream
          api_body
        }
      )
      switch(
        endpoint,
        "/email/batchWithTemplates" = {
          list("Messages" = out)
        },
        "/email/batch" = {
          out
        }
      )
    }
  )

  req_lst <- lapply(
    bdy_lst,
    function(body) {
      build_req_S7(client, endpoint, "POST") |>
        req_body_json(body)
    }
  )

  resp_lst <- req_perform_sequential(
    req_lst,
    on_error = "continue",
    progress = TRUE
  )

  Response(
    data = Reduce(
      rbind,
      lapply(resp_lst, function(x) resp_body_json(x, simplifyVector = TRUE))
    ),
    status = int_ply(resp_lst, function(x) resp_status(x)),
    request = req_lst,
    response = resp_lst,
    success = lgl_ply(resp_lst, function(x) isFALSE(resp_is_error(x)))
  )
}
