#' Get Postmark API Token
#'
#' Retrieves the Postmark API token from environment variables for
#' authentication with the Postmark API.
#'
#' @param env Character string specifying the environment. Either `"prod"` for
#'   production or `"test"` for testing. Defaults to `"prod"`.
#'
#' @return A character string containing the API token.
#'
#' @section Environment Variables:
#' The function looks for the following environment variables:
#' * `POSTMARK_PROD_SERVER_TOKEN` - for prod/production environment
#' * `POSTMARK_TEST_SERVER_TOKEN` - for test environment
#'
#' @section Test Mode:
#' When using `env = "test"`, you can configure a sandbox server in Postmark
#' that processes emails without actually delivering them. This is useful for
#' development, testing, and CI/CD pipelines. For more information, see the
#' [Postmark Sandbox Mode documentation](https://postmarkapp.com/developer/user-guide/sandbox-mode).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Set environment variable first
#' Sys.setenv(POSTMARK_SERVER_TOKEN = "your-token-here")
#'
#' # Get prod token
#' token <- get_token("prod")
#'
#' # Get test token
#' Sys.setenv(POSTMARK_TEST_SERVER_TOKEN = "your-test-token")
#' test_token <- get_token("test")
#' }
get_token <- function(env = c("prod", "test")) {
  env <- arg_match(env, env)
  envar <- switch(
    env,
    test = "POSTMARK_TEST_SERVER_TOKEN",
    prod = "POSTMARK_PROD_SERVER_TOKEN"
  )
  token <- Sys.getenv(envar)
  if (!nzchar(token)) {
    abort(
      c(
        "Cannot find token for API authentication.",
        i = sprintf(
          "Did you forget to set the `%s` env. variable?",
          envar
        )
      ),
      class = "postmarkr_error_missing_token"
    )
  }
  token
}
