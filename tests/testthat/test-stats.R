test_that("Stats class can be created with all parameters", {
  params <- Stats(
    tag = "welcome-email",
    fromdate = "2024-01-01",
    todate = "2024-01-31"
  )

  expect_true(S7_inherits(params, Stats))
  expect_equal(params@tag, "welcome-email")
  expect_equal(params@fromdate, "2024-01-01")
  expect_equal(params@todate, "2024-01-31")
})

test_that("Stats class can be created with no parameters", {
  params <- Stats()

  expect_true(S7_inherits(params, Stats))
  expect_equal(params@tag, character(0))
  expect_equal(params@fromdate, character(0))
  expect_equal(params@todate, character(0))
})

test_that("Stats class can be created with only tag", {
  params <- Stats(tag = "order-confirmation")

  expect_true(S7_inherits(params, Stats))
  expect_equal(params@tag, "order-confirmation")
  expect_equal(params@fromdate, character(0))
  expect_equal(params@todate, character(0))
})

test_that("Stats class can be created with only date range", {
  params <- Stats(
    fromdate = "2024-01-01",
    todate = "2024-12-31"
  )

  expect_true(S7_inherits(params, Stats))
  expect_equal(params@tag, character(0))
  expect_equal(params@fromdate, "2024-01-01")
  expect_equal(params@todate, "2024-12-31")
})

test_that("Stats accepts valid tag", {
  params <- Stats(tag = "test-tag")
  expect_equal(params@tag, "test-tag")
})

test_that("Stats rejects non-scalar tag", {
  expect_error(
    Stats(tag = c("tag1", "tag2")),
    class = "postmarkr_error_invalid_tag"
  )
})

test_that("Stats rejects empty string tag", {
  expect_error(
    Stats(tag = ""),
    class = "postmarkr_error_invalid_tag"
  )
})

test_that("Stats rejects numeric tag", {
  expect_error(Stats(tag = 123))
})

test_that("Stats accepts valid fromdate in YYYY-MM-DD format", {
  params <- Stats(fromdate = "2024-01-01")
  expect_equal(params@fromdate, "2024-01-01")
})

test_that("Stats accepts fromdate with different months", {
  params <- Stats(fromdate = "2024-12-31")
  expect_equal(params@fromdate, "2024-12-31")
})

test_that("Stats rejects fromdate without leading zeros", {
  expect_error(
    Stats(fromdate = "2024-1-1"),
    class = "postmarkr_error_invalid_fromdate"
  )
})

test_that("Stats rejects fromdate with time component", {
  expect_error(
    Stats(fromdate = "2024-01-01T00:00:00Z"),
    class = "postmarkr_error_invalid_fromdate"
  )
})

test_that("Stats rejects fromdate in wrong format", {
  expect_error(
    Stats(fromdate = "01/01/2024"),
    class = "postmarkr_error_invalid_fromdate"
  )
})

test_that("Stats rejects fromdate with invalid separator", {
  expect_error(
    Stats(fromdate = "2024.01.01"),
    class = "postmarkr_error_invalid_fromdate"
  )
})

test_that("Stats rejects non-scalar fromdate", {
  expect_error(
    Stats(fromdate = c("2024-01-01", "2024-12-31")),
    class = "postmarkr_error_invalid_fromdate"
  )
})

test_that("Stats rejects empty string fromdate", {
  expect_error(
    Stats(fromdate = ""),
    class = "postmarkr_error_invalid_fromdate"
  )
})

test_that("Stats rejects numeric fromdate", {
  expect_error(Stats(fromdate = 20240101))
})

test_that("Stats accepts valid todate in YYYY-MM-DD format", {
  params <- Stats(todate = "2024-12-31")
  expect_equal(params@todate, "2024-12-31")
})

test_that("Stats accepts todate with different months", {
  params <- Stats(todate = "2024-06-15")
  expect_equal(params@todate, "2024-06-15")
})

test_that("Stats rejects todate without leading zeros", {
  expect_error(
    Stats(todate = "2024-1-1"),
    class = "postmarkr_error_invalid_todate"
  )
})

test_that("Stats rejects todate with time component", {
  expect_error(
    Stats(todate = "2024-12-31T23:59:59Z"),
    class = "postmarkr_error_invalid_todate"
  )
})

test_that("Stats rejects todate in wrong format", {
  expect_error(
    Stats(todate = "12/31/2024"),
    class = "postmarkr_error_invalid_todate"
  )
})

test_that("Stats rejects todate with invalid separator", {
  expect_error(
    Stats(todate = "2024/12/31"),
    class = "postmarkr_error_invalid_todate"
  )
})

test_that("Stats rejects non-scalar todate", {
  expect_error(
    Stats(todate = c("2024-01-01", "2024-12-31")),
    class = "postmarkr_error_invalid_todate"
  )
})

test_that("Stats rejects empty string todate", {
  expect_error(
    Stats(todate = ""),
    class = "postmarkr_error_invalid_todate"
  )
})

test_that("Stats rejects numeric todate", {
  expect_error(Stats(todate = 20241231))
})

test_that("Stats validates multiple properties independently", {
  expect_error(
    Stats(
      tag = "valid-tag",
      fromdate = "invalid-date",
      todate = "2024-12-31"
    ),
    class = "postmarkr_error_invalid_fromdate"
  )
})

test_that("Stats validates all properties when all are invalid", {
  # Should fail on first validation error (tag is checked first)
  expect_error(
    Stats(
      tag = c("tag1", "tag2"),
      fromdate = "invalid-date",
      todate = "invalid-date"
    ),
    class = "postmarkr_error_invalid_tag"
  )
})

test_that("Stats accepts valid date range where fromdate is before todate", {
  params <- Stats(
    fromdate = "2024-01-01",
    todate = "2024-12-31"
  )
  expect_equal(params@fromdate, "2024-01-01")
  expect_equal(params@todate, "2024-12-31")
})

test_that("Stats accepts date range where fromdate equals todate", {
  params <- Stats(
    fromdate = "2024-06-15",
    todate = "2024-06-15"
  )
  expect_equal(params@fromdate, "2024-06-15")
  expect_equal(params@todate, "2024-06-15")
})

test_that("Stats rejects date range where fromdate is after todate", {
  expect_error(
    Stats(
      fromdate = "2024-12-31",
      todate = "2024-01-01"
    ),
    class = "postmarkr_error_invalid_date_range"
  )
})

test_that("Stats rejects date range where fromdate is one day after todate", {
  expect_error(
    Stats(
      fromdate = "2024-06-16",
      todate = "2024-06-15"
    ),
    class = "postmarkr_error_invalid_date_range"
  )
})

test_that("Stats allows fromdate without todate", {
  params <- Stats(fromdate = "2024-01-01")
  expect_equal(params@fromdate, "2024-01-01")
  expect_equal(params@todate, character(0))
})

test_that("Stats allows todate without fromdate", {
  params <- Stats(todate = "2024-12-31")
  expect_equal(params@fromdate, character(0))
  expect_equal(params@todate, "2024-12-31")
})

test_that("stats_get rejects non-scalar endpoint", {
  skip_if_offline()
  skip_if_not(nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")))

  client <- postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound",
    timeout = 30
  )

  params <- Stats()

  expect_error(
    stats_get(client, params, endpoint = c("overview", "sends")),
    class = "postmarkr_error_invalid_endpoint"
  )
})

test_that("stats_get rejects empty string endpoint", {
  skip_if_offline()
  skip_if_not(nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")))

  client <- postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound",
    timeout = 30
  )

  params <- Stats()

  expect_error(
    stats_get(client, params, endpoint = ""),
    class = "postmarkr_error_invalid_endpoint"
  )
})

test_that("stats_get rejects non-Stats params", {
  skip_if_offline()
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
  skip_if_offline()
  skip_if_not(nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")))

  client <- postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound",
    timeout = 30
  )

  params <- Stats()

  result <- stats_get(client, params)

  expect_true(S7_inherits(result, postmarkr_response))
  expect_equal(result@status, 200)
  expect_true(result@success)
  expect_true(S7_inherits(result@data, stats_overview_response))
})

test_that("stats_get works with specific endpoint", {
  skip_if_offline()
  skip_if_not(nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")))

  client <- postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound",
    timeout = 30
  )

  params <- Stats()

  result <- stats_get(client, params, endpoint = "sends")

  expect_true(S7_inherits(result, postmarkr_response))
  expect_equal(result@status, 200)
  expect_true(result@success)
  expect_s3_class(result@data@Days, "data.frame")
})

test_that("stats_get works with tag parameter", {
  skip_if_offline()
  skip_if_not(nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")))

  client <- postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound",
    timeout = 30
  )

  params <- Stats(tag = "test-tag")

  result <- stats_get(client, params)

  expect_true(S7_inherits(result, postmarkr_response))
  expect_equal(result@status, 200)
  expect_true(result@success)
})

test_that("stats_get works with date range parameters", {
  skip_if_offline()
  skip_if_not(nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")))

  client <- postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound",
    timeout = 30
  )

  params <- Stats(
    fromdate = "2024-01-01",
    todate = "2024-01-31"
  )

  result <- stats_get(client, params)

  expect_true(S7_inherits(result, postmarkr_response))
  expect_equal(result@status, 200)
  expect_true(result@success)
})

test_that("stats_get works with all parameters", {
  skip_if_offline()
  skip_if_not(nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")))

  client <- postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound",
    timeout = 30
  )

  params <- Stats(
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
  skip_if_offline()
  skip_if_not(nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")))

  client <- postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "broadcast",
    timeout = 30
  )

  params <- Stats()

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

  expect_true(S7_inherits(result), stats_timeseries_response)
  expect_true("Days" %in% prop_names(result))
  expect_true(is.data.frame(result@Days))
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
  skip_if_offline()
  skip_if_not(nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")))

  client <- postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound",
    timeout = 30
  )

  params <- Stats()

  result <- stats_get(client, params)

  # Should be validated as overview response
  expect_true(S7_inherits(result@data, stats_overview_response))
  expect_true(!is.null(result@data@Sent))
})

test_that("stats_get with endpoint returns validated time series data", {
  skip_if_offline()
  skip_if_not(nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")))

  client <- postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound",
    timeout = 30
  )

  params <- Stats()

  result <- stats_get(client, params, endpoint = "sends")

  expect_true("Days" %in% prop_names(result@data))
  expect_true(is.data.frame(result@data@Days))
  expect_true("Date" %in% colnames(result@data@Days))
})
