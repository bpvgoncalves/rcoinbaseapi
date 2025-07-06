test_that("transaction_list() returns a data.frame with expected fields", {

  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  # We store a fake key to allow it to be read before sending the request, so that the in-memory
  # key store is populated. A real key was used to obtain the response stored (and redacted) on
  # the mock directory.
  path <- apikey_store("fake_key", openssl::ec_keygen(), "abcd")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  apikey_read("abcd")

  with_mock_api({
    res <- transaction_list("f2a50186-34a5-4a92-95e1-5b6cf307bb35")
  })

  expect_s3_class(res, "data.frame")
  expect_true(nrow(res) > 0)
  expect_true(all(c("id", "status") %in% names(res)))  # adjust to match actual fields
})

test_that("transaction_list() validates bad UUIDs", {
  expect_error(transaction_list(NULL), "Invalid parameter `account_uuid`")
  expect_error(transaction_list(123), "Invalid parameter `account_uuid`")
  expect_error(transaction_list(c("abc", "def")), "Invalid parameter `account_uuid`")
  expect_error(transaction_list("bad-uuid"), "Invalid parameter `account_uuid`")
})


test_that("transaction_get() returns a named list with expected fields", {

  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  # We store a fake key to allow it to be read before sending the request, so that the in-memory
  # key store is populated. A real key was used to obtain the response stored (and redacted) on
  # the mock directory.
  path <- apikey_store("fake_key", openssl::ec_keygen(), "abcd")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  apikey_read("abcd")

  with_mock_api({
    res <- transaction_get("f2a50186-34a5-4a92-95e1-5b6cf307bb35",
                           "09da2e68-e414-433b-a734-d54faad9d456")
  })

  expect_type(res, "list")
  expect_named(res)
  expect_true("id" %in% names(res))
})

test_that("transaction_get() validates UUIDs", {
  expect_error(transaction_get(NULL, "f2a50186-34a5-4a92-95e1-5b6cf307bb35"),
               "Invalid parameter `account_uuid`")
  expect_error(transaction_get("f2a50186-34a5-4a92-95e1-5b6cf307bb35", NULL),
               "Invalid parameter `transaction_uuid`")
  expect_error(transaction_get("bad", "bad"), "Invalid parameter `account_uuid`")
})


test_that("transaction_summary() returns expected fields", {

  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  # We store a fake key to allow it to be read before sending the request, so that the in-memory
  # key store is populated. A real key was used to obtain the response stored (and redacted) on
  # the mock directory.
  path <- apikey_store("fake_key", openssl::ec_keygen(), "abcd")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  apikey_read("abcd")

  with_mock_api({
    res <- transaction_summary()
  })

  expect_type(res, "list")
  expect_true("total_volume" %in% names(res))
  expect_true("total_fees" %in% names(res))
  expect_true("fee_tier" %in% names(res))
})

test_that("transaction_summary() validates and applies parameters", {

  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  # We store a fake key to allow it to be read before sending the request, so that the in-memory
  # key store is populated. A real key was used to obtain the response stored (and redacted) on
  # the mock directory.
  path <- apikey_store("fake_key", openssl::ec_keygen(), "abcd")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  apikey_read("abcd")

  with_mock_api({
    res <- transaction_summary(product_type = "SPOT")
    expect_type(res, "list")

    expect_message(transaction_summary(product_type = "SPOT", expiry = "EXPIRING"),
                   "Ignoring parameter `expiry`")

    res2 <- transaction_summary(product_type = "FUTURE", expiry = "PERPETUAL")
    expect_type(res2, "list")

        res3 <- transaction_summary(venue = "CBE")
    expect_type(res3, "list")
  })
})

test_that("transaction_summary() rejects invalid inputs", {
  expect_error(transaction_summary(product_type = "XYZ"), "Invalid parameter `product_type`")
  expect_error(transaction_summary(expiry = "FOO"), "Invalid parameter `expiry`")
  expect_error(transaction_summary(venue = "123"), "Invalid parameter `venue`")
})
