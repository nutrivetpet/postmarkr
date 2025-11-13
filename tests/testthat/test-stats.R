test_that("stats class can be created with all parameters", {
  params <- stats(
    tag = "welcome-email",
    fromdate = "2024-01-01",
    todate = "2024-01-31"
  )

  expect_true(S7_inherits(params, stats))
  expect_equal(params@tag, "welcome-email")
  expect_equal(params@fromdate, "2024-01-01")
  expect_equal(params@todate, "2024-01-31")
})

test_that("stats class can be created with no parameters", {
  params <- stats()

  expect_true(S7_inherits(params, stats))
  expect_equal(params@tag, character(0))
  expect_equal(params@fromdate, character(0))
  expect_equal(params@todate, character(0))
})

test_that("stats class can be created with only tag", {
  params <- stats(tag = "order-confirmation")

  expect_true(S7_inherits(params, stats))
  expect_equal(params@tag, "order-confirmation")
  expect_equal(params@fromdate, character(0))
  expect_equal(params@todate, character(0))
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

test_that("stats validates multiple properties independently", {
  expect_error(
    stats(
      tag = "valid-tag",
      fromdate = "invalid-date",
      todate = "2024-12-31"
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
      todate = "invalid-date"
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
  skip_if_not(nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")))

  client <- postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound",
    timeout = 30
  )

  params <- stats()

  expect_error(
    stats_get(client, params, endpoint = c("overview", "sends")),
    class = "postmarkr_error_invalid_endpoint"
  )
})

test_that("stats_get rejects empty string endpoint", {
  skip_if_not(nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")))

  client <- postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound",
    timeout = 30
  )

  params <- stats()

  expect_error(
    stats_get(client, params, endpoint = ""),
    class = "postmarkr_error_invalid_endpoint"
  )
})

test_that("stats_get rejects non-stats params", {
  skip_if_not(nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")))

  client <- postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound",
    timeout = 30
  )

  expect_error(
    stats_get(client, list(tag = "test"))
  )
})

test_that("stats_get works with no endpoint and no params", {
  skip_if_not(nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")))

  client <- postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound",
    timeout = 30
  )

  params <- stats()

  result <- stats_get(client, params)

  expect_true(S7_inherits(result, postmarkr_response))
  expect_equal(result@status, 200)
  expect_true(result@success)
  expect_true(S7_inherits(result@data, stats_overview_response))
})

test_that("stats_get works with specific endpoint", {
  skip_if_not(nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")))

  client <- postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound",
    timeout = 30
  )

  params <- stats()

  result <- stats_get(client, params, endpoint = "sends")

  expect_true(S7_inherits(result, postmarkr_response))
  expect_equal(result@status, 200)
  expect_true(result@success)
  expect_type(result@data, "list")
})

test_that("stats_get works with tag parameter", {
  skip_if_not(nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")))

  client <- postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound",
    timeout = 30
  )

  params <- stats(tag = "test-tag")

  result <- stats_get(client, params)

  expect_true(S7_inherits(result, postmarkr_response))
  expect_equal(result@status, 200)
  expect_true(result@success)
})

test_that("stats_get works with date range parameters", {
  skip_if_not(nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")))

  client <- postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound",
    timeout = 30
  )

  params <- stats(
    fromdate = "2024-01-01",
    todate = "2024-01-31"
  )

  result <- stats_get(client, params)

  expect_true(S7_inherits(result, postmarkr_response))
  expect_equal(result@status, 200)
  expect_true(result@success)
})

test_that("stats_get works with all parameters", {
  skip_if_not(nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")))

  client <- postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound",
    timeout = 30
  )

  params <- stats(
    tag = "test-tag",
    fromdate = "2024-01-01",
    todate = "2024-01-31"
  )

  result <- stats_get(client, params, endpoint = "sends")

  expect_true(S7_inherits(result, postmarkr_response))
  expect_equal(result@status, 200)
  expect_true(result@success)
})

test_that("stats_get rejects with broadcast message stream", {
  skip_if_not(nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")))

  client <- postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "broadcast",
    timeout = 30
  )

  params <- stats()

  expect_error(stats_get(client, params), "404")
})

# Response validation tests

test_that("stats_overview_response can be created with valid data", {
  response <- stats_overview_response(
    Sent = 615L,
    Bounced = 64L,
    SMTPApiErrors = 25L,
    BounceRate = 10.406,
    SpamComplaints = 10L,
    SpamComplaintsRate = 1.626,
    Opens = 166L,
    UniqueOpens = 26L,
    Tracked = 111L,
    WithLinkTracking = 90L,
    WithOpenTracking = 51L,
    TotalTrackedLinksSent = 60L,
    UniqueLinksClicked = 30L,
    TotalClicks = 72L,
    WithClientRecorded = 14L,
    WithPlatformRecorded = 10L,
    WithReadTimeRecorded = 0L
  )

  expect_true(S7_inherits(response, stats_overview_response))
  expect_equal(response@Sent, 615L)
  expect_equal(response@BounceRate, 10.406)
})

test_that("stats_timeseries_response can be created with valid Days data.frame", {
  days <- data.frame(
    Date = c("2014-01-01", "2014-01-02"),
    Sent = c(140L, 160L)
  )

  response <- stats_timeseries_response(Days = days)

  expect_true(S7_inherits(response, stats_timeseries_response))
  expect_equal(nrow(response@Days), 2)
})

test_that("stats_timeseries_response validates Days data.frame structure", {
  invalid_days <- data.frame(
    NoDate = c("2014-01-01", "2014-01-02"),
    Sent = c(140L, 160L)
  )

  expect_error(
    stats_timeseries_response(Days = invalid_days),
    class = "postmarkr_error_invalid_stats_response"
  )
})

test_that("stats_timeseries_response rejects non-data.frame Days", {
  invalid_days <- list(
    list(Date = "2014-01-01", Sent = 140L)
  )

  expect_error(
    stats_timeseries_response(Days = invalid_days),
    class = "postmarkr_error_invalid_stats_response"
  )
})

test_that("validate_stats_response handles overview endpoint", {
  data <- list(
    Sent = 615L,
    Bounced = 64L,
    SMTPApiErrors = 25L,
    BounceRate = 10.406,
    SpamComplaints = 10L,
    SpamComplaintsRate = 1.626,
    Opens = 166L,
    UniqueOpens = 26L,
    Tracked = 111L,
    WithLinkTracking = 90L,
    WithOpenTracking = 51L,
    TotalTrackedLinksSent = 60L,
    UniqueLinksClicked = 30L,
    TotalClicks = 72L,
    WithClientRecorded = 14L,
    WithPlatformRecorded = 10L,
    WithReadTimeRecorded = 0L
  )

  result <- validate_stats_response(data, "/stats/outbound")

  expect_true(S7_inherits(result, stats_overview_response))
})

test_that("validate_stats_response handles time series endpoint", {
  data <- list(
    Days = data.frame(
      Date = c("2014-01-01", "2014-01-02"),
      Sent = c(140L, 160L)
    ),
    Sent = 615L
  )

  result <- validate_stats_response(data, "/stats/outbound/sends")

  expect_type(result, "list")
  expect_true("Days" %in% names(result))
  expect_true(is.data.frame(result$Days))
})

test_that("validate_stats_response errors on missing Days for time series", {
  data <- list(Sent = 615L)

  expect_error(
    validate_stats_response(data, "/stats/outbound/sends"),
    class = "postmarkr_error_stats_response_validation"
  )
})

test_that("validate_stats_response errors on malformed overview response", {
  data <- list(Sent = 615L) # Missing required fields

  expect_error(
    validate_stats_response(data, "/stats/outbound")
  )
})

test_that("stats_get returns validated data in response", {
  skip_if_not(nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")))

  client <- postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound",
    timeout = 30
  )

  params <- stats()

  result <- stats_get(client, params)

  # Should be validated as overview response
  expect_true(S7_inherits(result@data, stats_overview_response))
  expect_true(!is.null(result@data@Sent))
})

test_that("stats_get with endpoint returns validated time series data", {
  skip_if_not(nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")))

  client <- postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound",
    timeout = 30
  )

  params <- stats()

  result <- stats_get(client, params, endpoint = "sends")

  # Should be validated as time series response
  expect_type(result@data, "list")
  expect_true("Days" %in% names(result@data))
})
