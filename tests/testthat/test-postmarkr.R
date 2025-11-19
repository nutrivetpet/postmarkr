test_that("postmark class can be created with valid parameters", {
  client <- Client(
    token = "6777be1f-2a8f-4419-a8b4-fe6ff44900a0",
    message_stream = "outbound",
    timeout = 30
  )

  expect_true(S7_inherits(client, Client))
  expect_equal(client@token, "6777be1f-2a8f-4419-a8b4-fe6ff44900a0")
  expect_equal(client@message_stream, "outbound")
  expect_equal(client@timeout, 30)
})

test_that("postmark class uses default base_url", {
  client <- Client(
    token = "6777be1f-2a8f-4419-a8b4-fe6ff44900a0",
    message_stream = "outbound",
    timeout = 30
  )

  expect_equal(client@base_url, "https://api.postmarkapp.com")
})

test_that("postmark class accepts custom base_url", {
  client <- Client(
    token = "6777be1f-2a8f-4419-a8b4-fe6ff44900a0",
    message_stream = "outbound",
    base_url = "http://localhost:8080",
    timeout = 30
  )

  expect_equal(client@base_url, "http://localhost:8080")
})

test_that("postmark rejects invalid token format", {
  expect_error(
    Client(
      token = "invalid-token",
      message_stream = "outbound",
      timeout = 30
    ),
    class = "postmarkr_error_invalid_token"
  )
})

test_that("postmark rejects non-UUID token", {
  expect_error(
    Client(
      token = "not-a-uuid-at-all",
      message_stream = "outbound",
      timeout = 30
    ),
    class = "postmarkr_error_invalid_token"
  )
})

test_that("postmark accepts UUID with uppercase letters", {
  client <- Client(
    token = "6777BE1F-2A8F-4419-A8B4-FE6FF44900A0",
    message_stream = "outbound",
    timeout = 30
  )

  expect_true(S7_inherits(client, Client))
})

test_that("postmark accepts 'broadcast' message_stream", {
  client <- Client(
    token = "6777be1f-2a8f-4419-a8b4-fe6ff44900a0",
    message_stream = "broadcast",
    timeout = 30
  )

  expect_equal(client@message_stream, "broadcast")
})

test_that("postmark accepts 'outbound' message_stream", {
  client <- Client(
    token = "6777be1f-2a8f-4419-a8b4-fe6ff44900a0",
    message_stream = "outbound",
    timeout = 30
  )

  expect_equal(client@message_stream, "outbound")
})

test_that("postmark rejects invalid message_stream", {
  expect_error(
    Client(
      token = "6777be1f-2a8f-4419-a8b4-fe6ff44900a0",
      message_stream = "invalid",
      timeout = 30
    ),
    class = "postmarkr_error_invalid_message_stream"
  )
})

test_that("postmark rejects empty message_stream", {
  expect_error(
    Client(
      token = "6777be1f-2a8f-4419-a8b4-fe6ff44900a0",
      message_stream = "",
      timeout = 30
    ),
    class = "postmarkr_error_invalid_message_stream"
  )
})

test_that("postmark rejects base_url without protocol", {
  expect_error(
    Client(
      token = "6777be1f-2a8f-4419-a8b4-fe6ff44900a0",
      message_stream = "outbound",
      base_url = "api.postmarkapp.com",
      timeout = 30
    ),
    class = "postmarkr_error_invalid_base_url"
  )
})

test_that("postmark accepts https base_url", {
  client <- Client(
    token = "6777be1f-2a8f-4419-a8b4-fe6ff44900a0",
    message_stream = "outbound",
    base_url = "https://api.postmarkapp.com",
    timeout = 30
  )

  expect_equal(client@base_url, "https://api.postmarkapp.com")
})

test_that("postmark accepts http base_url", {
  client <- Client(
    token = "6777be1f-2a8f-4419-a8b4-fe6ff44900a0",
    message_stream = "outbound",
    base_url = "http://localhost:8080",
    timeout = 30
  )

  expect_equal(client@base_url, "http://localhost:8080")
})

test_that("postmark rejects timeout less than 1", {
  expect_error(
    Client(
      token = "6777be1f-2a8f-4419-a8b4-fe6ff44900a0",
      message_stream = "outbound",
      timeout = 0
    ),
    class = "postmarkr_error_invalid_timeout"
  )
})

test_that("postmark rejects negative timeout", {
  expect_error(
    Client(
      token = "6777be1f-2a8f-4419-a8b4-fe6ff44900a0",
      message_stream = "outbound",
      timeout = -5
    ),
    class = "postmarkr_error_invalid_timeout"
  )
})

test_that("postmark rejects non-integerish timeout", {
  expect_error(
    Client(
      token = "6777be1f-2a8f-4419-a8b4-fe6ff44900a0",
      message_stream = "outbound",
      timeout = 5.5
    ),
    class = "postmarkr_error_invalid_timeout"
  )
})

test_that("postmark accepts valid timeout values", {
  client1 <- Client(
    token = "6777be1f-2a8f-4419-a8b4-fe6ff44900a0",
    message_stream = "outbound",
    timeout = 1
  )

  client2 <- Client(
    token = "6777be1f-2a8f-4419-a8b4-fe6ff44900a0",
    message_stream = "outbound",
    timeout = 60
  )

  expect_equal(client1@timeout, 1)
  expect_equal(client2@timeout, 60)
})
