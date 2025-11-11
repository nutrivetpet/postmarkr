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
})

test_that("rep_list() works", {
  expect_identical(
    rep_list(list(a = "a", b = "b"), 2L),
    list(list(a = "a", b = "b"), list(a = "a", b = "b"))
  )
})
