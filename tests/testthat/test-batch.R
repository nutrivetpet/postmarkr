test_that("Batch can be created with Email messages", {
  emails <- list(
    Email(
      from = "sender@example.com",
      to = "recipient1@example.com",
      subject = "Test 1",
      text_body = "Body 1"
    ),
    Email(
      from = "sender@example.com",
      to = "recipient2@example.com",
      subject = "Test 2",
      text_body = "Body 2"
    )
  )

  batch <- Batch(messages = emails)

  expect_true(S7_inherits(batch, Batch))
  expect_equal(batch_size(batch), 2)
  expect_equal(batch_message_type(batch), "postmarkr::Email")
  expect_equal(batch@chunk_size, POSTMARK_MAX_BATCH_SIZE)
})

test_that("Batch can be created with Template messages", {
  templates <- list(
    Template(
      from = "sender@example.com",
      to = "recipient1@example.com",
      id = 12345678L,
      template_model = list(name = "Alice")
    ),
    Template(
      from = "sender@example.com",
      to = "recipient2@example.com",
      id = 12345678L,
      template_model = list(name = "Bob")
    )
  )

  batch <- Batch(messages = templates)

  expect_true(S7_inherits(batch, Batch))
  expect_equal(batch_size(batch), 2)
  expect_equal(batch_message_type(batch), "postmarkr::Email")
})

test_that("Batch can be created with custom chunk_size", {
  emails <- list(
    Email(
      from = "sender@example.com",
      to = "recipient@example.com",
      subject = "Test",
      text_body = "Body"
    )
  )

  batch <- Batch(messages = emails, chunk_size = 100L)

  expect_equal(batch@chunk_size, 100L)
})

test_that("Batch rejects empty messages list", {
  expect_error(
    Batch(messages = list()),
    class = "postmarkr_error_batch_empty"
  )
})

test_that("Batch rejects mixed Email and Template messages", {
  mixed <- list(
    Email(
      from = "sender@example.com",
      to = "recipient@example.com",
      subject = "Test",
      text_body = "Body"
    ),
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      id = 12345678L,
      template_model = list(name = "Alice")
    )
  )

  expect_error(
    Batch(messages = mixed),
    class = "postmarkr_error_batch_mixed_types"
  )
})

test_that("Batch rejects invalid message types", {
  invalid <- list(
    Email(
      from = "sender@example.com",
      to = "recipient@example.com",
      subject = "Test",
      text_body = "Body"
    ),
    "not an email",
    list(invalid = "object")
  )

  expect_error(
    Batch(messages = invalid),
    class = "postmarkr_error_batch_invalid_message_type"
  )
})

test_that("Batch rejects chunk_size > 500", {
  emails <- list(
    Email(
      from = "sender@example.com",
      to = "recipient@example.com",
      subject = "Test",
      text_body = "Body"
    )
  )

  expect_error(
    Batch(messages = emails, chunk_size = 501L),
    class = "postmarkr_error_batch_chunk_size_too_large"
  )
})

test_that("Batch rejects non-positive chunk_size", {
  emails <- list(
    Email(
      from = "sender@example.com",
      to = "recipient@example.com",
      subject = "Test",
      text_body = "Body"
    )
  )

  expect_error(
    Batch(messages = emails, chunk_size = 0L),
    class = "postmarkr_error_batch_invalid_chunk_size"
  )

  expect_error(
    Batch(messages = emails, chunk_size = -10L),
    class = "postmarkr_error_batch_invalid_chunk_size"
  )
})

test_that("Batch rejects non-integer chunk_size", {
  emails <- list(
    Email(
      from = "sender@example.com",
      to = "recipient@example.com",
      subject = "Test",
      text_body = "Body"
    )
  )

  expect_error(
    Batch(messages = emails, chunk_size = 100.5)
  )
})

test_that("batch_size returns correct count", {
  emails <- lapply(1:10, function(i) {
    Email(
      from = "sender@example.com",
      to = sprintf("recipient%d@example.com", i),
      subject = sprintf("Test %d", i),
      text_body = sprintf("Body %d", i)
    )
  })

  batch <- Batch(messages = emails)

  expect_equal(batch_size(batch), 10)
})

test_that("batch_chunk_count calculates chunks correctly", {
  emails_500 <- lapply(1:500, function(i) {
    Email(
      from = "sender@example.com",
      to = sprintf("recipient%d@example.com", i),
      subject = "Test",
      text_body = "Body"
    )
  })
  batch_500 <- Batch(messages = emails_500)
  expect_equal(batch_chunk_count(batch_500), 1)

  emails_1000 <- lapply(1:1000, function(i) {
    Email(
      from = "sender@example.com",
      to = sprintf("recipient%d@example.com", i),
      subject = "Test",
      text_body = "Body"
    )
  })
  batch_1000 <- Batch(messages = emails_1000)
  expect_equal(batch_chunk_count(batch_1000), 2)

  emails_750 <- lapply(1:750, function(i) {
    Email(
      from = "sender@example.com",
      to = sprintf("recipient%d@example.com", i),
      subject = "Test",
      text_body = "Body"
    )
  })
  batch_750 <- Batch(messages = emails_750)
  expect_equal(batch_chunk_count(batch_750), 2)

  batch_custom <- Batch(messages = emails_1000, chunk_size = 100L)
  expect_equal(batch_chunk_count(batch_custom), 10)
})

test_that("batch_message_type returns correct type", {
  emails <- list(
    Email(
      from = "sender@example.com",
      to = "recipient@example.com",
      subject = "Test",
      text_body = "Body"
    )
  )
  email_batch <- Batch(messages = emails)
  expect_equal(batch_message_type(email_batch), "postmarkr::Email")

  templates <- list(
    Template(
      from = "sender@example.com",
      to = "recipient@example.com",
      id = 12345678L,
      template_model = list(name = "Alice")
    )
  )
  template_batch <- Batch(messages = templates)
  expect_equal(batch_message_type(template_batch), "postmarkr::Template")
})

test_that("batch_get_chunks splits messages correctly", {
  emails <- lapply(1:1000, function(i) {
    Email(
      from = "sender@example.com",
      to = sprintf("recipient%d@example.com", i),
      subject = "Test",
      text_body = "Body"
    )
  })

  batch <- Batch(messages = emails, chunk_size = 500L)
  chunks <- batch_get_chunks(batch)

  expect_equal(length(chunks), 2)
  expect_equal(length(chunks[[1]]), 500)
  expect_equal(length(chunks[[2]]), 500)

  expect_equal(chunks[[1]][[1]]@to, "recipient1@example.com")
  expect_equal(chunks[[2]][[1]]@to, "recipient501@example.com")
})

test_that("batch_get_chunks handles partial last chunk", {
  emails <- lapply(1:750, function(i) {
    Email(
      from = "sender@example.com",
      to = sprintf("recipient%d@example.com", i),
      subject = "Test",
      text_body = "Body"
    )
  })

  batch <- Batch(messages = emails, chunk_size = 500L)
  chunks <- batch_get_chunks(batch)

  expect_equal(length(chunks), 2)
  expect_equal(length(chunks[[1]]), 500)
  expect_equal(length(chunks[[2]]), 250)
})

test_that("batch_get_chunks handles single chunk", {
  emails <- lapply(1:100, function(i) {
    Email(
      from = "sender@example.com",
      to = sprintf("recipient%d@example.com", i),
      subject = "Test",
      text_body = "Body"
    )
  })

  batch <- Batch(messages = emails, chunk_size = 500L)
  chunks <- batch_get_chunks(batch)

  expect_equal(length(chunks), 1)
  expect_equal(length(chunks[[1]]), 100)
})

test_that("batch_get_chunks handles custom chunk size", {
  emails <- lapply(1:1000, function(i) {
    Email(
      from = "sender@example.com",
      to = sprintf("recipient%d@example.com", i),
      subject = "Test",
      text_body = "Body"
    )
  })

  batch <- Batch(messages = emails, chunk_size = 100L)
  chunks <- batch_get_chunks(batch)

  expect_equal(length(chunks), 10)
  expect_true(all(int_ply(chunks, length) == 100))
})

test_that("batch_get_chunks preserves message order", {
  emails <- lapply(1:10, function(i) {
    Email(
      from = "sender@example.com",
      to = sprintf("recipient%d@example.com", i),
      subject = "Test",
      text_body = "Body"
    )
  })

  batch <- Batch(messages = emails, chunk_size = 3L)
  chunks <- batch_get_chunks(batch)

  expect_equal(length(chunks), 4)

  expect_equal(chunks[[1]][[1]]@to, "recipient1@example.com")
  expect_equal(chunks[[1]][[2]]@to, "recipient2@example.com")
  expect_equal(chunks[[1]][[3]]@to, "recipient3@example.com")
  expect_equal(chunks[[2]][[1]]@to, "recipient4@example.com")
  expect_equal(chunks[[4]][[1]]@to, "recipient10@example.com")
})

test_that("helper functions validate batch input", {
  not_a_batch <- list(a = 1, b = 2)

  expect_error(
    batch_size(not_a_batch),
    class = "postmarkr_error_not_batch_object"
  )

  expect_error(
    batch_chunk_count(not_a_batch),
    class = "postmarkr_error_not_batch_object"
  )

  expect_error(
    batch_message_type(not_a_batch),
    class = "postmarkr_error_not_batch_object"
  )

  expect_error(
    batch_get_chunks(not_a_batch),
    class = "postmarkr_error_not_batch_object"
  )
})

