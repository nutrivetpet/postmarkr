#' Send Email
#'
#' @description
#' Generic function to send emails through the Postmark API. This function
#' dispatches to the appropriate method based on the client and message types.
#'
#' @param client A [Postmarkr] client object.
#' @param message An [Email] object.
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
#' client <- Postmarkr(
#'   token = "your-server-token",
#'   message_stream = "outbound"
#' )
#'
#' # Create an email
#' email <- Email(
#'   from = "sender@example.com",
#'   to = "recipient@example.com",
#'   subject = "Hello from postmarkr",
#'   text_body = "This is a test email sent via R."
#' )
#'
#' # Send the email
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
#' html_email <- Email(
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
#' }
#'
#' @seealso
#' * [Postmarkr] for creating an API client
#' * [Email] for creating email objects
#' * \url{https://postmarkapp.com/developer/api/email-api#send-a-single-email}
#'   for Postmark API documentation
#'
#' @export
send <- new_generic("send", c("client", "email"), 
    function(client, template, ...) {
    S7_dispatch()
  }
)

method(send, list(Postmarkr, Email)) <- function(client, email, ...) {
  req <- build_req_S7(
    client = client,
    endpoint = "/email",
    method = "POST"
  )

  bdy <- as_api_body(email)
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

#' Send Template
#'
#' @description
#' Generic function to send template-based emails via the Postmark API.
#' This provides a structured way to send emails using predefined templates.
#'
#' @inheritParams send
#' @param template A [Template] object
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
#' client <- Postmarkr(
#'   token = "your-server-token",
#'   message_stream = "outbound"
#' )
#'
#' # Create a template
#' template <- Template(
#'   from = "sender@example.com",
#'   to = "recipient@example.com",
#'   id = 12345678L,
#'   template_model = list(name = "John", order_id = "ORD-123")
#' )
#'
#' # Send the template email
#' response <- send(client, template)
#' }
#'
#' @seealso
#' * [Postmarkr] for creating an API client
#' * [Template] for creating template objects
#' \url{https://postmarkapp.com/developer/api/templates-api#send-email-with-template}
#' for Postmark template API documentation
#'
#' @export
method(send, list(Postmarkr, Template)) <- function(
  client,
  template,
  ...
) {
  req <- build_req_S7(
      client = client,
      endpoint = "/email/withTemplate",
      method = "POST"
    )
  
  bdy <- as_api_body(template)
  bdy$MessageStream <- client@message_stream
  
  req <- req_body_json(bdy)

  resp <- req_perform(req)

  Response(
    data = resp_body_json(resp, simplifyVector = TRUE),
    status = resp_status(resp),
    request = req,
    response = resp,
    success = isFALSE(resp_is_error(resp))
  )
}