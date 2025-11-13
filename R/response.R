#' Response from Postmark API
#' Structured response object
#' @noRd
#' @keywords internals
postmarkr_response <- new_class(
  "postmarkr_response",
  properties = list(
    data = class_any,
    status = class_integer,
    request = class_any, # httr2_request
    response = class_any, # httr2_response
    success = class_logical
  )
)
