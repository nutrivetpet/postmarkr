test_that("build_req() works", {
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")),
    "Postmark API Key is missing"
  )
  expect_snapshot(build_req("/mock", "GET", "test"))
})
