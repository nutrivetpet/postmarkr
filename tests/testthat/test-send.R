test_that("send() works with Email using text body", {
  skip_if_offline()
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_FROM_EMAIL")),
    "Env. var. `POSTMARK_TEST_FROM_EMAIL` is missing"
  )
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")),
    "Postmark API Key is missing"
  )

  client <- Postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound"
  )

  email <- Email(
    from = Sys.getenv("POSTMARK_TEST_FROM_EMAIL"),
    to = "test@blackhole.postmarkapp.com",
    subject = "Test Email",
    text_body = "This is a plain text test email."
  )

  response <- send(client, email)

  expect_true(S7_inherits(response, Response))
  expect_true(response@success)
  expect_equal(response@status, 200L)
  expect_type(response@data, "list")
  expect_true(length(response@data) > 0)
})

test_that("send() works with Email using HTML body", {
  skip_if_offline()
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_FROM_EMAIL")),
    "Env. var. `POSTMARK_TEST_FROM_EMAIL` is missing"
  )
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")),
    "Postmark API Key is missing"
  )

  client <- Postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound"
  )

  email <- Email(
    from = Sys.getenv("POSTMARK_TEST_FROM_EMAIL"),
    to = "test@blackhole.postmarkapp.com",
    subject = "HTML Test Email",
    html_body = "<h1>Test</h1><p>This is an HTML email.</p>"
  )

  response <- send(client, email)

  expect_true(S7_inherits(response, Response))
  expect_true(response@success)
  expect_equal(response@status, 200L)
  expect_type(response@data, "list")
  expect_true(length(response@data) > 0)
})

test_that("send() works with Email with multiple recipients", {
  skip_if_offline()
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_FROM_EMAIL")),
    "Env. var. `POSTMARK_TEST_FROM_EMAIL` is missing"
  )
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")),
    "Postmark API Key is missing"
  )

  client <- Postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound"
  )

  email <- Email(
    from = Sys.getenv("POSTMARK_TEST_FROM_EMAIL"),
    to = c(
      "test@blackhole.postmarkapp.com",
      "test2@blackhole.postmarkapp.com"
    ),
    subject = "Multi-recipient Test",
    text_body = "Testing multiple recipients."
  )

  response <- send(client, email)

  expect_true(S7_inherits(response, Response))
  expect_true(response@success)
  expect_equal(response@status, 200L)
  expect_type(response@data, "list")
  expect_true(length(response@data) > 0)
})

test_that("send() works with Email with cc and bcc", {
  skip_if_offline()
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_FROM_EMAIL")),
    "Env. var. `POSTMARK_TEST_FROM_EMAIL` is missing"
  )
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")),
    "Postmark API Key is missing"
  )

  client <- Postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound"
  )

  email <- Email(
    from = Sys.getenv("POSTMARK_TEST_FROM_EMAIL"),
    to = "test@blackhole.postmarkapp.com",
    cc = "cc@blackhole.postmarkapp.com",
    bcc = "bcc@blackhole.postmarkapp.com",
    subject = "Test with CC/BCC",
    text_body = "Testing cc and bcc recipients."
  )

  response <- send(client, email)

  expect_true(S7_inherits(response, Response))
  expect_true(response@success)
  expect_equal(response@status, 200L)
  expect_type(response@data, "list")
  expect_true(length(response@data) > 0)
})

test_that("send() works with Email with all optional fields", {
  skip_if_offline()
  skip_on_cran()
  skip_if_not_installed("base64enc")
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_FROM_EMAIL")),
    "Env. var. `POSTMARK_TEST_FROM_EMAIL` is missing"
  )
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")),
    "Postmark API Key is missing"
  )

  client <- Postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound"
  )

  email <- Email(
    from = Sys.getenv("POSTMARK_TEST_FROM_EMAIL"),
    to = "test@blackhole.postmarkapp.com",
    cc = "cc@blackhole.postmarkapp.com",
    bcc = "bcc@blackhole.postmarkapp.com",
    subject = "Full Featured Email",
    html_body = "<h1>Test</h1><p>Full featured test.</p>",
    reply_to = "reply@example.com",
    tag = "test-tag",
    metadata = list(
      customer_id = "12345",
      campaign = "test-campaign"
    ),
    headers = list(
      list(Name = "X-Test-Header", Value = "test-value")
    ),
    track_opens = TRUE,
    track_links = "HtmlOnly",
    attachments = list(
      list(
        Name = "test.txt",
        Content = base64enc::base64encode(charToRaw("test content")),
        ContentType = "text/plain"
      )
    )
  )

  response <- send(client, email)

  expect_true(S7_inherits(response, Response))
  expect_true(response@success)
  expect_equal(response@status, 200L)
  expect_type(response@data, "list")
  expect_true(length(response@data) > 0)
})

test_that("send() works with Email with tracking enabled", {
  skip_if_offline()
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_FROM_EMAIL")),
    "Env. var. `POSTMARK_TEST_FROM_EMAIL` is missing"
  )
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")),
    "Postmark API Key is missing"
  )

  client <- Postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound"
  )

  email <- Email(
    from = Sys.getenv("POSTMARK_TEST_FROM_EMAIL"),
    to = "test@blackhole.postmarkapp.com",
    subject = "Tracked Email",
    html_body = "<p>Email with tracking <a href='https://example.com'>link</a></p>",
    track_opens = TRUE,
    track_links = "HtmlAndText"
  )

  response <- send(client, email)

  expect_true(response@success)
  expect_equal(response@status, 200L)
  expect_true(length(response@data) > 0)
})

test_that("send() includes message_stream from client in Email request", {
  skip_if_offline()
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_FROM_EMAIL")),
    "Env. var. `POSTMARK_TEST_FROM_EMAIL` is missing"
  )
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")),
    "Postmark API Key is missing"
  )

  client_outbound <- Postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound"
  )

  email <- Email(
    from = Sys.getenv("POSTMARK_TEST_FROM_EMAIL"),
    to = "test@blackhole.postmarkapp.com",
    subject = "Message Stream Test",
    text_body = "Testing message stream."
  )

  response_outbound <- send(client_outbound, email)
  expect_true(response_outbound@success)

  client_broadcast <- Postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "broadcast"
  )

  response_broadcast <- send(client_broadcast, email)
  expect_true(response_broadcast@success)
})

test_that("send() works with Email with custom headers", {
  skip_if_offline()
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_FROM_EMAIL")),
    "Env. var. `POSTMARK_TEST_FROM_EMAIL` is missing"
  )
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")),
    "Postmark API Key is missing"
  )

  client <- Postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound"
  )

  email <- Email(
    from = Sys.getenv("POSTMARK_TEST_FROM_EMAIL"),
    to = "test@blackhole.postmarkapp.com",
    subject = "Custom Headers Test",
    text_body = "Email with custom headers.",
    headers = list(
      list(Name = "X-Priority", Value = "1"),
      list(Name = "X-Custom-ID", Value = "TEST-123")
    )
  )

  response <- send(client, email)

  expect_true(response@success)
  expect_equal(response@status, 200L)
})

test_that("send() works with Email with metadata", {
  skip_if_offline()
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_FROM_EMAIL")),
    "Env. var. `POSTMARK_TEST_FROM_EMAIL` is missing"
  )
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")),
    "Postmark API Key is missing"
  )

  client <- Postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound"
  )

  email <- Email(
    from = Sys.getenv("POSTMARK_TEST_FROM_EMAIL"),
    to = "test@blackhole.postmarkapp.com",
    subject = "Metadata Test",
    text_body = "Email with metadata.",
    metadata = list(
      user_id = "user-123",
      order_id = "order-456",
      source = "R-package-test"
    )
  )

  response <- send(client, email)

  expect_true(response@success)
  expect_equal(response@status, 200L)
})

test_that("send() works with Email with reply_to", {
  skip_if_offline()
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_FROM_EMAIL")),
    "Env. var. `POSTMARK_TEST_FROM_EMAIL` is missing"
  )
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")),
    "Postmark API Key is missing"
  )

  client <- Postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound"
  )

  email <- Email(
    from = Sys.getenv("POSTMARK_TEST_FROM_EMAIL"),
    to = "test@blackhole.postmarkapp.com",
    subject = "Reply-To Test",
    text_body = "Email with reply-to address.",
    reply_to = "replies@example.com"
  )

  response <- send(client, email)

  expect_true(response@success)
  expect_equal(response@status, 200L)
})

test_that("send() works with Email with tag", {
  skip_if_offline()
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_FROM_EMAIL")),
    "Env. var. `POSTMARK_TEST_FROM_EMAIL` is missing"
  )
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")),
    "Postmark API Key is missing"
  )

  client <- Postmarkr(
    token = Sys.getenv("POSTMARK_TEST_SERVER_TOKEN"),
    message_stream = "outbound"
  )

  email <- Email(
    from = Sys.getenv("POSTMARK_TEST_FROM_EMAIL"),
    to = "test@blackhole.postmarkapp.com",
    subject = "Tag Test",
    text_body = "Email with tag.",
    tag = "test-email-tag"
  )

  response <- send(client, email)

  expect_true(response@success)
  expect_equal(response@status, 200L)
})

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
  expect_true(length(response@data) > 0)

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
  expect_type(response@data, "list")
  expect_true(length(response@data) > 0)
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
  expect_true(length(response@data) > 0)
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
  expect_type(response@data, "list")
  expect_true(length(response@data) > 0)
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

  template_untracked <- Template(
    from = Sys.getenv("POSTMARK_TEST_FROM_EMAIL"),
    to = "test@blackhole.postmarkapp.com",
    id = as.integer(Sys.getenv("POSTMARK_TEST_TEMPLATE_ID")),
    template_model = list(test = "value")
  )

  response2 <- send(client, template_untracked)
  expect_true(response2@success)
})
