#' Send an email via Postmark
#'
#' @description
#' Generic function to send emails through the Postmark API. This function
#' dispatches to the appropriate method based on the client and message types.
#'
#' @param client A [Postmarkr] client object configured with API token and
#'   message stream.
#' @param message An [Email] object containing the email content and
#'   configuration to be sent.
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
#' @details
#' This generic function provides a clean interface for sending emails through
#' Postmark. The function:
#' \itemize{
#'   \item Converts the Email object to the Postmark API JSON format
#'   \item Adds the message stream from the client configuration
#'   \item Performs the HTTP POST request to the Postmark API
#'   \item Returns a structured response object with detailed information
#' }
#'
#' The `MessageID` in the response can be used to track the email status
#' via webhooks or the bounce API.
#'
#' **Error Handling:**
#' If the API request fails, the function will throw an error with details
#' from the Postmark API response. Common error codes include:
#' \itemize{
#'   \item 300: Invalid email address
#'   \item 401: Unauthorized (invalid API token)
#'   \item 406: Inactive recipient (hard bounce)
#'   \item 422: Unprocessable entity (validation error)
#'   \item 429: Too many requests (rate limited)
#'   \item 500: Internal server error
#' }
#'
#' @examples
#' \dontrun{
#' # Create a Postmark client
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
send <- new_generic("send", c("client", "message"))

method(send, list(Postmarkr, Email)) <- function(client, message) {
  req <- build_req_s7(
    client = client,
    endpoint = "/email",
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
