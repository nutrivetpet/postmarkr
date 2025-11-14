# Template Class Validation Tests ----------------------------------------

test_that("Template requires either id or alias", {
  expect_error(
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      template_model = list(name = "test")
    ),
    class = "postmarkr_error_template_missing_identifier"
  )
})

test_that("Template rejects both id and alias together", {
  expect_error(
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      id = 12345L,
      alias = "welcome-email",
      template_model = list(name = "test")
    ),
    class = "postmarkr_error_template_conflicting_identifiers"
  )
})

test_that("Template accepts valid id without alias", {
  expect_no_error(
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      id = 12345L,
      template_model = list(name = "test")
    )
  )
})

test_that("Template accepts valid alias without id", {
  expect_no_error(
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      alias = "welcome-email",
      template_model = list(name = "test")
    )
  )
})

test_that("Template rejects invalid id type (numeric when expecting integer)", {
  # S7 will throw a simpleError for type mismatches
  expect_error(
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      id = 12345.5,  # numeric, not integer
      template_model = list(name = "test")
    )
  )
})

test_that("Template rejects non-positive id", {
  expect_error(
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      id = 0L,
      template_model = list(name = "test")
    ),
    class = "postmarkr_error_invalid_template_id"
  )

  expect_error(
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      id = -1L,
      template_model = list(name = "test")
    ),
    class = "postmarkr_error_invalid_template_id"
  )
})

test_that("Template rejects non-scalar alias", {
  expect_error(
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      alias = c("welcome", "onboarding"),
      template_model = list(name = "test")
    ),
    class = "postmarkr_error_invalid_alias"
  )
})

test_that("Template rejects empty alias", {
  expect_error(
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      alias = "",
      template_model = list(name = "test")
    ),
    class = "postmarkr_error_invalid_alias"
  )
})

test_that("Template requires template_model", {
  expect_error(
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      id = 12345L
    ),
    class = "postmarkr_error_template_missing_model"
  )
})

test_that("Template requires named template_model", {
  expect_error(
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      id = 12345L,
      template_model = list("unnamed", "values")
    ),
    class = "postmarkr_error_invalid_template_model"
  )
})

test_that("Template accepts valid nested template_model", {
  expect_no_error(
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      id = 12345L,
      template_model = list(
        user_name = "John",
        company = list(name = "ACME")
      )
    )
  )
})

test_that("Template rejects invalid tag (too long)", {
  expect_error(
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      id = 12345L,
      template_model = list(name = "test"),
      tag = paste(rep("a", 1001), collapse = "")
    ),
    class = "postmarkr_error_invalid_tag"
  )
})

test_that("Template rejects invalid tag (empty string)", {
  expect_error(
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      id = 12345L,
      template_model = list(name = "test"),
      tag = ""
    ),
    class = "postmarkr_error_invalid_tag"
  )
})

test_that("Template rejects non-scalar tag", {
  expect_error(
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      id = 12345L,
      template_model = list(name = "test"),
      tag = c("tag1", "tag2")
    ),
    class = "postmarkr_error_invalid_tag"
  )
})

test_that("Template accepts valid tag", {
  expect_no_error(
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      id = 12345L,
      template_model = list(name = "test"),
      tag = "welcome-email"
    )
  )
})

test_that("Template rejects invalid track_links value", {
  expect_error(
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      id = 12345L,
      template_model = list(name = "test"),
      track_links = "Invalid"
    ),
    class = "postmarkr_error_email_invalid_track_links"
  )
})

test_that("Template accepts valid track_links values", {
  valid_options <- c("None", "HtmlAndText", "HtmlOnly", "TextOnly")

  for (option in valid_options) {
    expect_no_error(
      Template(
        from = "sender@example.com",
        to = "recipient@example.com",
        id = 12345L,
        template_model = list(name = "test"),
        track_links = option
      )
    )
  }
})

test_that("Template rejects too many recipients", {
  # Create 51 recipients (exceeds max of 50)
  too_many <- paste0("user", 1:51, "@example.com")

  expect_error(
    Template(
      from = "sender@example.com",
      to = too_many,
      id = 12345L,
      template_model = list(name = "test")
    ),
    class = "postmarkr_error_template_too_many_recipients"
  )
})

test_that("Template rejects too many recipients across to/cc/bcc", {
  expect_error(
    Template(
      from = "sender@example.com",
      to = paste0("user", 1:20, "@example.com"),
      cc = paste0("cc", 1:20, "@example.com"),
      bcc = paste0("bcc", 1:11, "@example.com"),  # Total = 51
      id = 12345L,
      template_model = list(name = "test")
    ),
    class = "postmarkr_error_template_too_many_recipients"
  )
})

test_that("Template accepts exactly 50 recipients", {
  expect_no_error(
    Template(
      from = "sender@example.com",
      to = paste0("user", 1:50, "@example.com"),
      id = 12345L,
      template_model = list(name = "test")
    )
  )
})

test_that("Template accepts 50 recipients split across to/cc/bcc", {
  expect_no_error(
    Template(
      from = "sender@example.com",
      to = paste0("user", 1:20, "@example.com"),
      cc = paste0("cc", 1:20, "@example.com"),
      bcc = paste0("bcc", 1:10, "@example.com"),  # Total = 50
      id = 12345L,
      template_model = list(name = "test")
    )
  )
})

# Integration Tests -------------------------------------------------------

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

# send() method tests -----------------------------------------------------

test_that("send() works with Template using template ID", {
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

  client <- Postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "broadcast"
  )

  template <- Template(
    from = Sys.getenv("POSTMARK_TEST_FROM_EMAIL"),
    to = "test@blackhole.postmarkapp.com",
    id = as.integer(Sys.getenv("POSTMARK_TEST_TEMPLATE_ID")),
    template_model = list(test = "value")
  )

  response <- send(client, template)

  expect_true(S7_inherits(response, Response))
  expect_true(response@success)
  expect_equal(response@status, 200L)
  expect_type(response@data, "list")
  expect_true("MessageID" %in% names(response@data))
  expect_true("To" %in% names(response@data))
  expect_true("SubmittedAt" %in% names(response@data))
})

test_that("send() works with Template using template alias", {
  skip_if_offline()
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_FROM_EMAIL")),
    "Env. var. `POSTMARK_TEST_FROM_EMAIL` is missing"
  )
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_TEMPLATE_ALIAS")),
    "Env. var. `POSTMARK_TEST_TEMPLATE_ALIAS` is missing"
  )
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")),
    "Postmark API Key is missing"
  )

  client <- Postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "broadcast"
  )

  template <- Template(
    from = Sys.getenv("POSTMARK_TEST_FROM_EMAIL"),
    to = "test@blackhole.postmarkapp.com",
    alias = Sys.getenv("POSTMARK_TEST_TEMPLATE_ALIAS"),
    template_model = list(test = "value")
  )

  response <- send(client, template)

  expect_true(S7_inherits(response, Response))
  expect_true(response@success)
  expect_equal(response@status, 200L)
  expect_type(response@data, "list")
  expect_true("MessageID" %in% names(response@data))
})

test_that("send() works with Template with multiple recipients", {
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

  client <- Postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "broadcast"
  )

  template <- Template(
    from = Sys.getenv("POSTMARK_TEST_FROM_EMAIL"),
    to = c(
      "test@blackhole.postmarkapp.com",
      "test2@blackhole.postmarkapp.com"
    ),
    id = as.integer(Sys.getenv("POSTMARK_TEST_TEMPLATE_ID")),
    template_model = list(test = "value")
  )

  response <- send(client, template)

  expect_true(S7_inherits(response, Response))
  expect_true(response@success)
  expect_equal(response@status, 200L)
})

test_that("send() works with Template with all optional fields", {
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

  client <- Postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "broadcast"
  )

  template <- Template(
    from = Sys.getenv("POSTMARK_TEST_FROM_EMAIL"),
    to = "test@blackhole.postmarkapp.com",
    cc = "cc@blackhole.postmarkapp.com",
    bcc = "bcc@blackhole.postmarkapp.com",
    id = as.integer(Sys.getenv("POSTMARK_TEST_TEMPLATE_ID")),
    template_model = list(
      name = "Test User",
      company = list(name = "Test Company")
    ),
    inline_css = TRUE,
    tag = "test-tag",
    reply_to = "reply@example.com",
    headers = list(
      list(Name = "X-Test-Header", Value = "test-value")
    ),
    track_opens = TRUE,
    track_links = "HtmlOnly",
    metadata = list(
      customer_id = "12345",
      campaign = "test"
    )
  )

  response <- send(client, template)

  expect_true(S7_inherits(response, Response))
  expect_true(response@success)
  expect_equal(response@status, 200L)
  expect_type(response@data, "list")
})

test_that("send() works with Template with nested template_model", {
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

  client <- Postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "broadcast"
  )

  template <- Template(
    from = Sys.getenv("POSTMARK_TEST_FROM_EMAIL"),
    to = "test@blackhole.postmarkapp.com",
    id = as.integer(Sys.getenv("POSTMARK_TEST_TEMPLATE_ID")),
    template_model = list(
      user = list(
        name = "John Doe",
        email = "john@example.com"
      ),
      order = list(
        id = "ORD-123",
        items = list(
          list(name = "Item 1", price = 10.00),
          list(name = "Item 2", price = 20.00)
        )
      )
    )
  )

  response <- send(client, template)

  expect_true(S7_inherits(response, Response))
  expect_true(response@success)
  expect_equal(response@status, 200L)
})

test_that("send() includes message_stream from client in Template request", {
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

  # Test with outbound stream
  client_outbound <- Postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound"
  )

  template <- Template(
    from = Sys.getenv("POSTMARK_TEST_FROM_EMAIL"),
    to = "test@blackhole.postmarkapp.com",
    id = as.integer(Sys.getenv("POSTMARK_TEST_TEMPLATE_ID")),
    template_model = list(test = "value")
  )

  response_outbound <- send(client_outbound, template)
  expect_true(response_outbound@success)

  # Test with broadcast stream
  client_broadcast <- Postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "broadcast"
  )

  response_broadcast <- send(client_broadcast, template)
  expect_true(response_broadcast@success)
})

test_that("send() with Template respects tracking settings", {
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

  client <- Postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "broadcast"
  )

  # Test with tracking enabled
  template_tracked <- Template(
    from = Sys.getenv("POSTMARK_TEST_FROM_EMAIL"),
    to = "test@blackhole.postmarkapp.com",
    id = as.integer(Sys.getenv("POSTMARK_TEST_TEMPLATE_ID")),
    template_model = list(test = "value"),
    track_opens = TRUE,
    track_links = "HtmlAndText"
  )

  response <- send(client, template_tracked)
  expect_true(response@success)

  # Test without tracking
  template_untracked <- Template(
    from = Sys.getenv("POSTMARK_TEST_FROM_EMAIL"),
    to = "test@blackhole.postmarkapp.com",
    id = as.integer(Sys.getenv("POSTMARK_TEST_TEMPLATE_ID")),
    template_model = list(test = "value")
  )

  response2 <- send(client, template_untracked)
  expect_true(response2@success)
})
