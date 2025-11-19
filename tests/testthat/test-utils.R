test_that("extract_email() extracts email from name-formatted strings", {
  expect_identical(
    extract_email("John Doe <john@example.com>"),
    "john@example.com"
  )
  expect_identical(
    extract_email("simple@example.com"),
    "simple@example.com"
  )
  expect_identical(
    extract_email(c("Alice <alice@example.com>", "bob@example.com")),
    c("alice@example.com", "bob@example.com")
  )
})

test_that("validate_email() accepts valid email formats", {
  expect_invisible(expect_null(validate_email("user@example.com")))
  expect_invisible(expect_null(validate_email("user.name@example.com")))
  expect_invisible(expect_null(validate_email("user+tag@example.co.uk")))
  expect_invisible(expect_null(validate_email("user_name@sub.example.com")))
  expect_invisible(expect_null(validate_email("John Doe <john@example.com>")))
  expect_invisible(expect_null(validate_email(c(
    "alice@example.com",
    "bob@example.org"
  ))))
})

test_that("validate_email() rejects invalid email formats", {
  expect_error(
    validate_email("not-an-email"),
    class = "postmarkr_error_invalid_email"
  )
  expect_error(
    validate_email("missing@domain"),
    class = "postmarkr_error_invalid_email"
  )
  expect_error(
    validate_email("@example.com"),
    class = "postmarkr_error_invalid_email"
  )
  expect_error(
    validate_email("user@"),
    class = "postmarkr_error_invalid_email"
  )
  expect_error(
    validate_email("user @example.com"),
    class = "postmarkr_error_invalid_email"
  )
  expect_error(
    validate_email(c("valid@example.com", "invalid")),
    class = "postmarkr_error_invalid_email"
  )
})
