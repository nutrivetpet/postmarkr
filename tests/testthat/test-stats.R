test_that("stats_outbound_overview_impl() works", {
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")),
    "Postmark API Key is missing"
  )
  expect_type(
    stats_outbound_overview_impl("test"),
    "list"
  )
})
