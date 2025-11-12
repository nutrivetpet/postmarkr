test_that("email class accepts valid html_body email", {
  result <- email(
    from = "sender@example.com",
    to = "recipient@example.com",
    html_body = "<h1>Hello</h1>"
  )
  expect_true(S7_inherits(result, email))
})

test_that("email class accepts valid text_body email", {
  result <- email(
    from = "sender@example.com",
    to = "recipient@example.com",
    text_body = "Hello world"
  )
  expect_true(S7_inherits(result, email))
})

test_that("email class rejects both html_body and text_body", {
  expect_error(
    email(
      from = "sender@example.com",
      to = "recipient@example.com",
      html_body = "<h1>Hello</h1>",
      text_body = "Hello world"
    ),
    class = "postmarkr_email_body_conflict"
  )
})

test_that("email class rejects neither html_body nor text_body", {
  expect_error(
    email(
      from = "sender@example.com",
      to = "recipient@example.com"
    ),
    class = "postmarkr_email_missing_body"
  )
})

test_that("email class accepts maximum recipients (50 total)", {
  result <- email(
    from = "sender@example.com",
    to = rep("user@example.com", 30),
    cc = rep("cc@example.com", 15),
    bcc = rep("bcc@example.com", 5),
    html_body = "<h1>Hello</h1>"
  )
  expect_true(S7_inherits(result, email))
})

test_that("email class rejects over 50 total recipients", {
  expect_error(
    email(
      from = "sender@example.com",
      to = rep("user@example.com", 30),
      cc = rep("cc@example.com", 15),
      bcc = rep("bcc@example.com", 6),
      html_body = "<h1>Hello</h1>"
    ),
    class = "postmarkr_email_too_many_recipients"
  )
})

test_that("email class accepts all optional fields", {
  result <- email(
    from = "sender@example.com",
    to = "recipient@example.com",
    cc = "cc@example.com",
    bcc = "bcc@example.com",
    subject = "Test Subject",
    tag = "test-tag",
    html_body = "<h1>Hello</h1>",
    reply_to = "reply@example.com",
    metadata = list(customer_id = "12345"),
    headers = list(list(Name = "X-Priority", Value = "High")),
    track_opens = TRUE,
    track_links = "HtmlOnly",
    attachments = list(
      list(
        Name = "test.pdf",
        Content = "base64content",
        ContentType = "application/pdf"
      )
    )
  )
  expect_true(S7_inherits(result, email))
})

test_that("email class accepts multiple recipients in to field", {
  result <- email(
    from = "sender@example.com",
    to = c("user1@example.com", "user2@example.com", "user3@example.com"),
    html_body = "<h1>Hello</h1>"
  )
  expect_true(S7_inherits(result, email))
})

test_that("email class accepts empty optional fields", {
  result <- email(
    from = "sender@example.com",
    to = "recipient@example.com",
    html_body = "<h1>Hello</h1>",
    cc = character(),
    bcc = character(),
    subject = character(),
    tag = character(),
    reply_to = character(),
    metadata = list(),
    headers = list(),
    track_opens = logical(),
    track_links = character(),
    attachments = list()
  )
  expect_true(S7_inherits(result, email))
})

test_that("email class with 50 recipients in to field only", {
  result <- email(
    from = "sender@example.com",
    to = rep("user@example.com", 50),
    html_body = "<h1>Hello</h1>"
  )
  expect_true(S7_inherits(result, email))
})

test_that("email class with 51 recipients in to field only fails", {
  expect_error(
    email(
      from = "sender@example.com",
      to = rep("user@example.com", 51),
      html_body = "<h1>Hello</h1>"
    ),
    class = "postmarkr_email_too_many_recipients"
  )
})

test_that("email class with mixed recipient distribution", {
  result <- email(
    from = "sender@example.com",
    to = rep("to@example.com", 10),
    cc = rep("cc@example.com", 20),
    bcc = rep("bcc@example.com", 20),
    text_body = "Hello"
  )
  expect_true(S7_inherits(result, email))
})
