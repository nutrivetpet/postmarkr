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

test_that("template() helper requires template_model argument", {
  expect_error(
    template(
      from = "sender@example.com",
      to = "recipient@example.com",
      id = 12345L
    ),
    class = "postmarkr_error_missing_template_model"
  )
})

test_that("Template accepts empty template_model list", {
  expect_no_error(
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      id = 12345L,
      template_model = list()
    )
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

test_that("Template accepts name-formatted email addresses", {
  expect_no_error(
    Template(
      from = "John Doe <sender@example.com>",
      to = "Jane Smith <recipient@example.com>",
      id = 12345L,
      template_model = list(name = "test")
    )
  )
})

test_that("Template rejects invalid from email", {
  expect_error(
    Template(
      from = "not-an-email",
      to = "recipient@example.com",
      id = 12345L,
      template_model = list(name = "test")
    ),
    class = "postmarkr_error_invalid_email"
  )
})

test_that("Template rejects invalid to email", {
  expect_error(
    Template(
      from = "sender@example.com",
      to = "invalid-email",
      id = 12345L,
      template_model = list(name = "test")
    ),
    class = "postmarkr_error_invalid_email"
  )
})

test_that("Template rejects invalid cc email", {
  expect_error(
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      cc = "bad@",
      id = 12345L,
      template_model = list(name = "test")
    ),
    class = "postmarkr_error_invalid_email"
  )
})

test_that("Template rejects invalid bcc email", {
  expect_error(
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      bcc = "@example.com",
      id = 12345L,
      template_model = list(name = "test")
    ),
    class = "postmarkr_error_invalid_email"
  )
})

test_that("Template rejects invalid reply_to email", {
  expect_error(
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      reply_to = "missing-domain@",
      id = 12345L,
      template_model = list(name = "test")
    ),
    class = "postmarkr_error_invalid_email"
  )
})

test_that("Template rejects mixed valid and invalid emails in to", {
  expect_error(
    Template(
      from = "sender@example.com",
      to = c("valid@example.com", "invalid"),
      id = 12345L,
      template_model = list(name = "test")
    ),
    class = "postmarkr_error_invalid_email"
  )
})
