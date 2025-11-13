test_that("template_send_email_impl() works", {
  skip_if_offline()
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_FROM_EMAIL")),
    "Env. var. `POSTMARK_TEST_FROM_EMAIL` is missing"
  )
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_TEMPLATE_ID")),
    "Env. var. `POSTMARK_TEST_TEMPLATE_ID` is missing"
  )
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")),
    "Postmark API Key is missing"
  )
  out <- template_send_email_impl(
    from = Sys.getenv("POSTMARK_TEST_FROM_EMAIL"),
    to = "test@blackhole.postmarkapp.com",
    id = as.integer(Sys.getenv("POSTMARK_TEST_TEMPLATE_ID")),
    template_model = list(this = "is a test"),
    msg_stream = "broadcast",
    tag = "test",
    env = "test",
    track_opens = TRUE
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1L)
  expect_equal(ncol(out), 5L)
})
