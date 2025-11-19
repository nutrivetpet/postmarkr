test_that("as_api_body converts Email with html_body to API format", {
  e <- Email(
    from = "sender@example.com",
    to = "recipient@example.com",
    subject = "Test Subject",
    html_body = "<p>Hello World</p>"
  )

  result <- as_api_body(e)

  expect_type(result, "list")
  expect_named(result, c("From", "To", "Subject", "HtmlBody"))
  expect_equal(result$From, "sender@example.com")
  expect_equal(result$To, "recipient@example.com")
  expect_equal(result$Subject, "Test Subject")
  expect_equal(result$HtmlBody, "<p>Hello World</p>")
  expect_false("TextBody" %in% names(result))
})

test_that("as_api_body converts Email with text_body to API format", {
  e <- Email(
    from = "sender@example.com",
    to = "recipient@example.com",
    subject = "Test Subject",
    text_body = "Hello World"
  )

  result <- as_api_body(e)

  expect_type(result, "list")
  expect_named(result, c("From", "To", "Subject", "TextBody"))
  expect_equal(result$From, "sender@example.com")
  expect_equal(result$To, "recipient@example.com")
  expect_equal(result$Subject, "Test Subject")
  expect_equal(result$TextBody, "Hello World")
  expect_false("HtmlBody" %in% names(result))
})

test_that("as_api_body converts multiple recipients to comma-separated string", {
  e <- Email(
    from = "sender@example.com",
    to = c(
      "recipient1@example.com",
      "recipient2@example.com",
      "recipient3@example.com"
    ),
    text_body = "Hello"
  )

  result <- as_api_body(e)

  expect_equal(
    result$To,
    "recipient1@example.com, recipient2@example.com, recipient3@example.com"
  )
})

test_that("as_api_body includes Cc when present", {
  e <- Email(
    from = "sender@example.com",
    to = "recipient@example.com",
    cc = c("cc1@example.com", "cc2@example.com"),
    text_body = "Hello"
  )

  result <- as_api_body(e)

  expect_true("Cc" %in% names(result))
  expect_equal(result$Cc, "cc1@example.com, cc2@example.com")
})

test_that("as_api_body includes Bcc when present", {
  e <- Email(
    from = "sender@example.com",
    to = "recipient@example.com",
    bcc = "bcc@example.com",
    text_body = "Hello"
  )

  result <- as_api_body(e)

  expect_true("Bcc" %in% names(result))
  expect_equal(result$Bcc, "bcc@example.com")
})

test_that("as_api_body includes ReplyTo when present", {
  e <- Email(
    from = "sender@example.com",
    to = "recipient@example.com",
    reply_to = "reply@example.com",
    text_body = "Hello"
  )

  result <- as_api_body(e)

  expect_true("ReplyTo" %in% names(result))
  expect_equal(result$ReplyTo, "reply@example.com")
})

test_that("as_api_body includes Tag when present", {
  e <- Email(
    from = "sender@example.com",
    to = "recipient@example.com",
    tag = "welcome-Email",
    text_body = "Hello"
  )

  result <- as_api_body(e)

  expect_true("Tag" %in% names(result))
  expect_equal(result$Tag, "welcome-Email")
})

test_that("as_api_body includes TrackOpens when present", {
  e <- Email(
    from = "sender@example.com",
    to = "recipient@example.com",
    html_body = "<p>Hello</p>",
    track_opens = TRUE
  )

  result <- as_api_body(e)

  expect_true("TrackOpens" %in% names(result))
  expect_equal(result$TrackOpens, TRUE)
})

test_that("as_api_body includes TrackLinks when present", {
  e <- Email(
    from = "sender@example.com",
    to = "recipient@example.com",
    html_body = "<p>Hello</p>",
    track_links = "HtmlAndText"
  )

  result <- as_api_body(e)

  expect_true("TrackLinks" %in% names(result))
  expect_equal(result$TrackLinks, "HtmlAndText")
})

test_that("as_api_body includes Metadata when present", {
  e <- Email(
    from = "sender@example.com",
    to = "recipient@example.com",
    text_body = "Hello",
    metadata = list(
      customer_id = "12345",
      campaign = "onboarding"
    )
  )

  result <- as_api_body(e)

  expect_true("Metadata" %in% names(result))
  expect_type(result$Metadata, "list")
  expect_equal(result$Metadata$customer_id, "12345")
  expect_equal(result$Metadata$campaign, "onboarding")
})

test_that("as_api_body includes Headers when present", {
  e <- Email(
    from = "sender@example.com",
    to = "recipient@example.com",
    text_body = "Hello",
    headers = list(
      list(Name = "X-Priority", Value = "High")
    )
  )

  result <- as_api_body(e)

  expect_true("Headers" %in% names(result))
  expect_type(result$Headers, "list")
  expect_equal(result$Headers[[1]]$Name, "X-Priority")
  expect_equal(result$Headers[[1]]$Value, "High")
})

test_that("as_api_body includes Attachments when present", {
  e <- Email(
    from = "sender@example.com",
    to = "recipient@example.com",
    text_body = "Hello",
    attachments = list(
      list(
        Name = "document.pdf",
        Content = "base64content",
        ContentType = "application/pdf"
      )
    )
  )

  result <- as_api_body(e)

  expect_true("Attachments" %in% names(result))
  expect_type(result$Attachments, "list")
  expect_equal(result$Attachments[[1]]$Name, "document.pdf")
  expect_equal(result$Attachments[[1]]$Content, "base64content")
  expect_equal(result$Attachments[[1]]$ContentType, "application/pdf")
})

test_that("as_api_body omits optional fields when not present", {
  e <- Email(
    from = "sender@example.com",
    to = "recipient@example.com",
    text_body = "Hello"
  )

  result <- as_api_body(e)

  expect_false("Cc" %in% names(result))
  expect_false("Bcc" %in% names(result))
  expect_false("ReplyTo" %in% names(result))
  expect_false("Tag" %in% names(result))
  expect_false("TrackOpens" %in% names(result))
  expect_false("TrackLinks" %in% names(result))
  expect_false("Metadata" %in% names(result))
  expect_false("Headers" %in% names(result))
  expect_false("Attachments" %in% names(result))
})

test_that("as_api_body handles complex Email with all fields", {
  e <- Email(
    from = "sender@example.com",
    to = c("recipient1@example.com", "recipient2@example.com"),
    cc = "cc@example.com",
    bcc = "bcc@example.com",
    subject = "Complete Email Test",
    html_body = "<h1>Hello</h1><p>World</p>",
    reply_to = "reply@example.com",
    tag = "test-email",
    track_opens = TRUE,
    track_links = "HtmlOnly",
    metadata = list(test = "value"),
    headers = list(list(Name = "X-Test", Value = "test")),
    attachments = list(list(
      Name = "file.txt",
      Content = "abc",
      ContentType = "text/plain"
    ))
  )

  result <- as_api_body(e)

  expected_names <- c(
    "From",
    "To",
    "Subject",
    "Cc",
    "Bcc",
    "ReplyTo",
    "Tag",
    "HtmlBody",
    "TrackOpens",
    "TrackLinks",
    "Metadata",
    "Headers",
    "Attachments"
  )
  expect_true(all(expected_names %in% names(result)))

  # Verify values
  expect_equal(result$From, "sender@example.com")
  expect_equal(result$To, "recipient1@example.com, recipient2@example.com")
  expect_equal(result$Cc, "cc@example.com")
  expect_equal(result$Bcc, "bcc@example.com")
  expect_equal(result$Subject, "Complete Email Test")
  expect_equal(result$HtmlBody, "<h1>Hello</h1><p>World</p>")
  expect_equal(result$ReplyTo, "reply@example.com")
  expect_equal(result$Tag, "test-email")
  expect_equal(result$TrackOpens, TRUE)
  expect_equal(result$TrackLinks, "HtmlOnly")
})

test_that("as_api_body uses camelCase for all property names", {
  e <- Email(
    from = "sender@example.com",
    to = "recipient@example.com",
    html_body = "<p>Test</p>",
    reply_to = "reply@example.com",
    track_opens = TRUE,
    track_links = "HtmlAndText"
  )

  result <- as_api_body(e)

  expect_false(any(grepl("_", names(result))))
})
