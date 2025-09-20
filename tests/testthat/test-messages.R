test_that("outbound_messages_fetch_impl() works", {
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")),
    "Postmark API Key is missing"
  )
  out <- outbound_messages_fetch_impl(count = 1L, offset = 1L, env = "test")
  expect_type(out, "list")
  expect_length(out, 2L)
})

test_that("outbound_messages_collect() works", {
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")),
    "Postmark API Key is missing"
  )
  out <- outbound_messages_collect_impl(env = "test")
  expect_s3_class(out, "data.frame")
})
