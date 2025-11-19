test_that("build_req_S7() works", {
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")),
    "Postmark API Key is missing"
  )
  client <- client(
    Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound"
  )
  expect_snapshot(build_req_S7(client, "/mock", "GET"))
})
