test_that("address_list() works with valid account UUID", {

  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  # We store a fake key to allow it to be read before sending the request, so that the in-memory
  # key store is populated. A real key was used to obtain the response stored (and redacted) on
  # the mock directory.
  path <- apikey_store("fake_key", openssl::ec_keygen(), "abcd")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  apikey_read("abcd")

  with_mock_api({
    result <- address_list("c48f20b0-1234-4bd8-90c2-abcde1234567")

    expect_s3_class(result, "data.frame")
    expect_true("id" %in% names(result))  # assuming the response has an "id" column
  })

})

test_that("address_list() errors on invalid UUID", {

  expect_error(address_list(NULL),
               "Invalid parameter `account_uuid`")
  expect_error(address_list(123),
               "Invalid parameter `account_uuid`")
  expect_error(address_list(NA),
               "Invalid parameter `account_uuid`")
  expect_error(address_list(NaN),
               "Invalid parameter `account_uuid`")
  expect_error(address_list("not-a-uuid"),
               "Invalid parameter `account_uuid`")

})


test_that("address_transactions_list() returns transactions data", {

  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  # We store a fake key to allow it to be read before sending the request, so that the in-memory
  # key store is populated. A real key was used to obtain the response stored (and redacted) on
  # the mock directory.
  path <- apikey_store("fake_key", openssl::ec_keygen(), "abcd")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  apikey_read("abcd")

  with_mock_api({
    result <- address_transactions_list("c48f20b0-1234-4bd8-90c2-abcde1234567",
                                        "dd3183eb-af1d-5f5d-a90d-cbff946435ff")

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) >= 0)
  })
})

test_that("address_transactions_list() errors on invalid UUIDs", {

  expect_error(address_transactions_list(NULL, "dd3183eb-af1d-5f5d-a90d-cbff946435ff"),
               "Invalid parameter `account_uuid`")
  expect_error(address_transactions_list(NA, "dd3183eb-af1d-5f5d-a90d-cbff946435ff"),
               "Invalid parameter `account_uuid`")
  expect_error(address_transactions_list(123, "dd3183eb-af1d-5f5d-a90d-cbff946435ff"),
               "Invalid parameter `account_uuid`")
  expect_error(address_transactions_list("invalid", "dd3183eb-af1d-5f5d-a90d-cbff946435ff"),
               "Invalid parameter `account_uuid`")

  expect_error(address_transactions_list("c48f20b0-1234-4bd8-90c2-abcde1234567", NULL),
               "Invalid parameter `address_uuid`")
  expect_error(address_transactions_list("c48f20b0-1234-4bd8-90c2-abcde1234567", NA),
               "Invalid parameter `address_uuid`")
  expect_error(address_transactions_list("c48f20b0-1234-4bd8-90c2-abcde1234567", 123),
               "Invalid parameter `address_uuid`")
  expect_error(address_transactions_list("c48f20b0-1234-4bd8-90c2-abcde1234567", "invalid"),
               "Invalid parameter `address_uuid`")
})


test_that("address_new() works with and without label", {

  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  # We store a fake key to allow it to be read before sending the request, so that the in-memory
  # key store is populated. A real key was used to obtain the response stored (and redacted) on
  # the mock directory.
  path <- apikey_store("fake_key", openssl::ec_keygen(), "abcd")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  apikey_read("abcd")

  with_mock_api({

    # With label
    result1 <- address_new("c48f20b0-1234-4bd8-90c2-abcde1234567", label = "new address")
    expect_type(result1, "list")
    expect_true("id" %in% names(result1))

    # Without label
    result2 <- address_new("c48f20b0-1234-4bd8-90c2-abcde1234567")
    expect_type(result2, "list")
  })
})

test_that("address_new() errors on invalid UUID or label", {

  expect_error(address_new(NULL), "Invalid parameter `account_uuid`")
  expect_error(address_new(NA), "Invalid parameter `account_uuid`")
  expect_error(address_new(123), "Invalid parameter `account_uuid`")
  expect_error(address_new("invalid-uuid"), "Invalid parameter `account_uuid`")


  expect_error(address_new("c48f20b0-1234-4bd8-90c2-abcde1234567", label = list(1, 2)),
               "Invalid parameter `label`")
})
