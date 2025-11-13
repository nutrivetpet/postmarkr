test_that("stats class can be created with all parameters", {
  params <- stats(
    tag = "welcome-email",
    fromdate = "2024-01-01",
    todate = "2024-01-31",
    messagestream = "outbound"
  )

  expect_true(S7_inherits(params, stats))
  expect_equal(params@tag, "welcome-email")
  expect_equal(params@fromdate, "2024-01-01")
  expect_equal(params@todate, "2024-01-31")
  expect_equal(params@messagestream, "outbound")
})

test_that("stats class can be created with no parameters", {
  params <- stats()

  expect_true(S7_inherits(params, stats))
  expect_equal(params@tag, character(0))
  expect_equal(params@fromdate, character(0))
  expect_equal(params@todate, character(0))
  expect_equal(params@messagestream, character(0))
})

test_that("stats class can be created with only tag", {
  params <- stats(tag = "order-confirmation")

  expect_true(S7_inherits(params, stats))
  expect_equal(params@tag, "order-confirmation")
  expect_equal(params@fromdate, character(0))
  expect_equal(params@todate, character(0))
  expect_equal(params@messagestream, character(0))
})

test_that("stats class can be created with only date range", {
  params <- stats(
    fromdate = "2024-01-01",
    todate = "2024-12-31"
  )

  expect_true(S7_inherits(params, stats))
  expect_equal(params@tag, character(0))
  expect_equal(params@fromdate, "2024-01-01")
  expect_equal(params@todate, "2024-12-31")
  expect_equal(params@messagestream, character(0))
})

test_that("stats class can be created with only messagestream", {
  params <- stats(messagestream = "broadcast")

  expect_true(S7_inherits(params, stats))
  expect_equal(params@tag, character(0))
  expect_equal(params@fromdate, character(0))
  expect_equal(params@todate, character(0))
  expect_equal(params@messagestream, "broadcast")
})

test_that("stats accepts valid tag", {
  params <- stats(tag = "test-tag")
  expect_equal(params@tag, "test-tag")
})

test_that("stats rejects non-scalar tag", {
  expect_error(
    stats(tag = c("tag1", "tag2")),
    class = "postmarkr_error_invalid_tag"
  )
})

test_that("stats rejects empty string tag", {
  expect_error(
    stats(tag = ""),
    class = "postmarkr_error_invalid_tag"
  )
})

test_that("stats rejects numeric tag", {
  expect_error(stats(tag = 123))
})

test_that("stats accepts valid fromdate in YYYY-MM-DD format", {
  params <- stats(fromdate = "2024-01-01")
  expect_equal(params@fromdate, "2024-01-01")
})

test_that("stats accepts fromdate with different months", {
  params <- stats(fromdate = "2024-12-31")
  expect_equal(params@fromdate, "2024-12-31")
})

test_that("stats rejects fromdate without leading zeros", {
  expect_error(
    stats(fromdate = "2024-1-1"),
    class = "postmarkr_error_invalid_fromdate"
  )
})

test_that("stats rejects fromdate with time component", {
  expect_error(
    stats(fromdate = "2024-01-01T00:00:00Z"),
    class = "postmarkr_error_invalid_fromdate"
  )
})

test_that("stats rejects fromdate in wrong format", {
  expect_error(
    stats(fromdate = "01/01/2024"),
    class = "postmarkr_error_invalid_fromdate"
  )
})

test_that("stats rejects fromdate with invalid separator", {
  expect_error(
    stats(fromdate = "2024.01.01"),
    class = "postmarkr_error_invalid_fromdate"
  )
})

test_that("stats rejects non-scalar fromdate", {
  expect_error(
    stats(fromdate = c("2024-01-01", "2024-12-31")),
    class = "postmarkr_error_invalid_fromdate"
  )
})

test_that("stats rejects empty string fromdate", {
  expect_error(
    stats(fromdate = ""),
    class = "postmarkr_error_invalid_fromdate"
  )
})

test_that("stats rejects numeric fromdate", {
  expect_error(stats(fromdate = 20240101))
})

test_that("stats accepts valid todate in YYYY-MM-DD format", {
  params <- stats(todate = "2024-12-31")
  expect_equal(params@todate, "2024-12-31")
})

test_that("stats accepts todate with different months", {
  params <- stats(todate = "2024-06-15")
  expect_equal(params@todate, "2024-06-15")
})

test_that("stats rejects todate without leading zeros", {
  expect_error(
    stats(todate = "2024-1-1"),
    class = "postmarkr_error_invalid_todate"
  )
})

test_that("stats rejects todate with time component", {
  expect_error(
    stats(todate = "2024-12-31T23:59:59Z"),
    class = "postmarkr_error_invalid_todate"
  )
})

test_that("stats rejects todate in wrong format", {
  expect_error(
    stats(todate = "12/31/2024"),
    class = "postmarkr_error_invalid_todate"
  )
})

test_that("stats rejects todate with invalid separator", {
  expect_error(
    stats(todate = "2024/12/31"),
    class = "postmarkr_error_invalid_todate"
  )
})

test_that("stats rejects non-scalar todate", {
  expect_error(
    stats(todate = c("2024-01-01", "2024-12-31")),
    class = "postmarkr_error_invalid_todate"
  )
})

test_that("stats rejects empty string todate", {
  expect_error(
    stats(todate = ""),
    class = "postmarkr_error_invalid_todate"
  )
})

test_that("stats rejects numeric todate", {
  expect_error(stats(todate = 20241231))
})

test_that("stats accepts 'outbound' messagestream", {
  params <- stats(messagestream = "outbound")
  expect_equal(params@messagestream, "outbound")
})

test_that("stats accepts 'broadcast' messagestream", {
  params <- stats(messagestream = "broadcast")
  expect_equal(params@messagestream, "broadcast")
})

test_that("stats rejects invalid messagestream", {
  expect_error(
    stats(messagestream = "invalid"),
    class = "postmarkr_error_invalid_messagestream"
  )
})

test_that("stats rejects 'transactional' messagestream", {
  expect_error(
    stats(messagestream = "transactional"),
    class = "postmarkr_error_invalid_messagestream"
  )
})

test_that("stats rejects empty string messagestream", {
  expect_error(
    stats(messagestream = ""),
    class = "postmarkr_error_invalid_messagestream"
  )
})

test_that("stats rejects non-scalar messagestream", {
  expect_error(
    stats(messagestream = c("outbound", "broadcast")),
    class = "postmarkr_error_invalid_messagestream"
  )
})

test_that("stats rejects numeric messagestream", {
  expect_error(stats(messagestream = 123))
})

test_that("stats validates multiple properties independently", {
  expect_error(
    stats(
      tag = "valid-tag",
      fromdate = "invalid-date",
      todate = "2024-12-31",
      messagestream = "outbound"
    ),
    class = "postmarkr_error_invalid_fromdate"
  )
})

test_that("stats validates all properties when all are invalid", {
  # Should fail on first validation error (tag is checked first)
  expect_error(
    stats(
      tag = c("tag1", "tag2"),
      fromdate = "invalid-date",
      todate = "invalid-date",
      messagestream = "invalid"
    ),
    class = "postmarkr_error_invalid_tag"
  )
})

test_that("stats accepts valid date range where fromdate is before todate", {
  params <- stats(
    fromdate = "2024-01-01",
    todate = "2024-12-31"
  )
  expect_equal(params@fromdate, "2024-01-01")
  expect_equal(params@todate, "2024-12-31")
})

test_that("stats accepts date range where fromdate equals todate", {
  params <- stats(
    fromdate = "2024-06-15",
    todate = "2024-06-15"
  )
  expect_equal(params@fromdate, "2024-06-15")
  expect_equal(params@todate, "2024-06-15")
})

test_that("stats rejects date range where fromdate is after todate", {
  expect_error(
    stats(
      fromdate = "2024-12-31",
      todate = "2024-01-01"
    ),
    class = "postmarkr_error_invalid_date_range"
  )
})

test_that("stats rejects date range where fromdate is one day after todate", {
  expect_error(
    stats(
      fromdate = "2024-06-16",
      todate = "2024-06-15"
    ),
    class = "postmarkr_error_invalid_date_range"
  )
})

test_that("stats allows fromdate without todate", {
  params <- stats(fromdate = "2024-01-01")
  expect_equal(params@fromdate, "2024-01-01")
  expect_equal(params@todate, character(0))
})

test_that("stats allows todate without fromdate", {
  params <- stats(todate = "2024-12-31")
  expect_equal(params@fromdate, character(0))
  expect_equal(params@todate, "2024-12-31")
})

test_that("stats_get rejects non-scalar endpoint", {
  token <- Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")
  skip_if(!nzchar(token), "POSTMARK_TEST_SERVER_TOKEN not set")

  client <- postmarkr(
    token = token,
    message_stream = "outbound",
    timeout = 30
  )

  expect_error(
    stats_get(client, c("overview", "sends")),
    class = "postmarkr_error_invalid_endpoint"
  )
})

test_that("stats_get rejects empty string endpoint", {
  token <- Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")
  skip_if(!nzchar(token), "POSTMARK_TEST_SERVER_TOKEN not set")

  client <- postmarkr(
    token = token,
    message_stream = "outbound",
    timeout = 30
  )

  expect_error(
    stats_get(client, ""),
    class = "postmarkr_error_invalid_endpoint"
  )
})

test_that("stats_get rejects numeric endpoint", {
  token <- Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")
  skip_if(!nzchar(token), "POSTMARK_TEST_SERVER_TOKEN not set")

  client <- postmarkr(
    token = token,
    message_stream = "outbound",
    timeout = 30
  )

  expect_error(
    stats_get(client, 123)
  )
})

test_that("stats_get rejects non-stats params object", {
  token <- Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")
  skip_if(!nzchar(token), "POSTMARK_TEST_SERVER_TOKEN not set")

  client <- postmarkr(
    token = token,
    message_stream = "outbound",
    timeout = 30
  )

  expect_error(
    stats_get(client, "overview", params = list(tag = "test")),
    class = "postmarkr_error_invalid_params"
  )
})

test_that("stats_get rejects string params", {
  token <- Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")
  skip_if(!nzchar(token), "POSTMARK_TEST_SERVER_TOKEN not set")

  client <- postmarkr(
    token = token,
    message_stream = "outbound",
    timeout = 30
  )

  expect_error(
    stats_get(client, "overview", params = "invalid"),
    class = "postmarkr_error_invalid_params"
  )
})

test_that("stats_get works with valid endpoint and no params", {
  token <- Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")
  skip_if(!nzchar(token), "POSTMARK_TEST_SERVER_TOKEN not set")

  client <- postmarkr(
    token = token,
    message_stream = "outbound",
    timeout = 30
  )

  result <- stats_get(client, "overview")

  expect_true(S7_inherits(result, postmarkr_response))
  expect_true(is.list(result@data))
  expect_equal(result@status, 200)
  expect_true(result@success)
})

test_that("stats_get works with NULL params", {
  token <- Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")
  skip_if(!nzchar(token), "POSTMARK_TEST_SERVER_TOKEN not set")

  client <- postmarkr(
    token = token,
    message_stream = "outbound",
    timeout = 30
  )

  result <- stats_get(client, "overview", params = NULL)

  expect_true(S7_inherits(result, postmarkr_response))
  expect_true(result@success)
})

test_that("stats_get works with empty stats params", {
  token <- Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")
  skip_if(!nzchar(token), "POSTMARK_TEST_SERVER_TOKEN not set")

  client <- postmarkr(
    token = token,
    message_stream = "outbound",
    timeout = 30
  )

  params <- stats()
  result <- stats_get(client, "overview", params = params)

  expect_true(S7_inherits(result, postmarkr_response))
  expect_true(result@success)
})

test_that("stats_get works with tag param", {
  token <- Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")
  skip_if(!nzchar(token), "POSTMARK_TEST_SERVER_TOKEN not set")

  client <- postmarkr(
    token = token,
    message_stream = "outbound",
    timeout = 30
  )

  params <- stats(tag = "test-tag")
  result <- stats_get(client, "overview", params = params)

  expect_true(S7_inherits(result, postmarkr_response))
  expect_true(result@success)
})

test_that("stats_get works with date range params", {
  token <- Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")
  skip_if(!nzchar(token), "POSTMARK_TEST_SERVER_TOKEN not set")

  client <- postmarkr(
    token = token,
    message_stream = "outbound",
    timeout = 30
  )

  params <- stats(
    fromdate = "2024-01-01",
    todate = "2024-01-31"
  )
  result <- stats_get(client, "overview", params = params)

  expect_true(S7_inherits(result, postmarkr_response))
  expect_true(result@success)
})

test_that("stats_get works with messagestream param", {
  token <- Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")
  skip_if(!nzchar(token), "POSTMARK_TEST_SERVER_TOKEN not set")

  client <- postmarkr(
    token = token,
    message_stream = "outbound",
    timeout = 30
  )

  params <- stats(messagestream = "outbound")
  result <- stats_get(client, "overview", params = params)

  expect_true(S7_inherits(result, postmarkr_response))
  expect_true(result@success)
})

test_that("stats_get works with all params", {
  token <- Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")
  skip_if(!nzchar(token), "POSTMARK_TEST_SERVER_TOKEN not set")

  client <- postmarkr(
    token = token,
    message_stream = "outbound",
    timeout = 30
  )

  params <- stats(
    tag = "test-tag",
    fromdate = "2024-01-01",
    todate = "2024-01-31",
    messagestream = "outbound"
  )
  result <- stats_get(client, "overview", params = params)

  expect_true(S7_inherits(result, postmarkr_response))
  expect_true(result@success)
})

test_that("stats_get works with different endpoints", {
  token <- Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")
  skip_if(!nzchar(token), "POSTMARK_TEST_SERVER_TOKEN not set")

  client <- postmarkr(
    token = token,
    message_stream = "outbound",
    timeout = 30
  )

  result1 <- stats_get(client, "overview")
  expect_true(S7_inherits(result1, postmarkr_response))
  expect_true(result1@success)

  result2 <- stats_get(client, "sends")
  expect_true(S7_inherits(result2, postmarkr_response))
  expect_true(result2@success)

  result3 <- stats_get(client, "bounces")
  expect_true(S7_inherits(result3, postmarkr_response))
  expect_true(result3@success)
})

test_that("stats_get works with nested endpoint path", {
  token <- Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")
  skip_if(!nzchar(token), "POSTMARK_TEST_SERVER_TOKEN not set")

  client <- postmarkr(
    token = token,
    message_stream = "outbound",
    timeout = 30
  )

  result <- stats_get(client, "opens/emailclients")

  expect_true(S7_inherits(result, postmarkr_response))
  expect_true(result@success)
})

test_that("stats_get works with broadcast message stream", {
  token <- Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")
  skip_if(!nzchar(token), "POSTMARK_TEST_SERVER_TOKEN not set")

  client <- postmarkr(
    token = token,
    message_stream = "broadcast",
    timeout = 30
  )

  result <- stats_get(client, "overview")

  expect_true(S7_inherits(result, postmarkr_response))
  expect_true(result@success)
})
