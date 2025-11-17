test_that("get_token() works", {
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("POSTMARK_TEST_SERVER_TOKEN")),
    "Postmark API Key is missing"
  )
  expect_true(is_scalar_character(get_token("test")))
})

test_that("generate_offset_batches() works", {
  expect_identical(
    generate_offset_batches(1500L),
    data.frame(count = 500L, offset = seq(0L, 1500L, by = 500L))
  )
})

test_that("capitalize_first() works", {
  expect_identical(capitalize_first("test"), "Test")
  expect_identical(
    capitalize_first(c("test01", "test02")),
    c("Test01", "Test02")
  )
  expect_identical(
    capitalize_first(c("test01", "")),
    c("Test01", "")
  )
})

test_that("rep_list() works", {
  expect_identical(
    rep_list(list(a = "a", b = "b"), 2L),
    list(list(a = "a", b = "b"), list(a = "a", b = "b"))
  )
})

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
  expect_invisible(expect_null(validate_email(c("alice@example.com", "bob@example.org"))))
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
