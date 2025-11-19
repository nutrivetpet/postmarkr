test_that("get_token() works", {
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")),
    "Postmark API Key is missing"
  )
  expect_true(is_scalar_character(get_token("test")))
})
