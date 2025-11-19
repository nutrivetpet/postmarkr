#' Response from Postmark API
#' Structured response object
#' @noRd
#' @keywords internal
Response <- new_class(
  "Response",
  properties = list(
    data = class_any,
    status = class_integer,
    request = class_any, # httr2_request
    response = class_any, #  httr2_response
    success = class_logical
  )
)
