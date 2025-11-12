pstmrk_abort <- function(
  message,
  ...,
  class = character(),
  call = caller_env()
) {
  abort(
    message = message,
    ...,
    class = c(class, "postmarkr_error"),
    call = call
  )
}
