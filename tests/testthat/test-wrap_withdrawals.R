test_that("withdraw_list() returns data with valid arguments", {

  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  # We store a fake key to allow it to be read before sending the request, so that the in-memory
  # key store is populated. A real key was used to obtain the response stored (and redacted) on
  # the mock directory.
  path <- apikey_store("fake_key", openssl::ec_keygen(), "abcd")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  apikey_read("abcd")

  with_mock_api({
    res <- withdraw_list("f2a50186-34a5-4a92-95e1-5b6cf307bb35")
    expect_s3_class(res, "data.frame")
    expect_true(nrow(res) >= 0)
  })
})


test_that("withdraw_new() returns data with valid arguments", {

  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  # We store a fake key to allow it to be read before sending the request, so that the in-memory
  # key store is populated. A real key was used to obtain the response stored (and redacted) on
  # the mock directory.
  path <- apikey_store("fake_key", openssl::ec_keygen(), "abcd")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  apikey_read("abcd")

  with_mock_api({
    res <- withdraw_new("f2a50186-34a5-4a92-95e1-5b6cf307bb35",
                        amount = 10,
                        currency = "USD",
                        pay_method_uuid = "83562370-3e5c-51db-87da-752af5ab9559")

    expect_type(res, "list")
    expect_named(res)
    expect_true("id" %in% names(res))
  })
})


test_that("withdraw_confirm() returns data with valid arguments", {

  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  # We store a fake key to allow it to be read before sending the request, so that the in-memory
  # key store is populated. A real key was used to obtain the response stored (and redacted) on
  # the mock directory.
  path <- apikey_store("fake_key", openssl::ec_keygen(), "abcd")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  apikey_read("abcd")

  with_mock_api({
    res <- withdraw_confirm("f2a50186-34a5-4a92-95e1-5b6cf307bb35",
                            "97c0dcf2-daf7-446c-8dcc-66a9b72b6391")

    expect_type(res, "list")
    expect_named(res)
    expect_true("status" %in% names(res))
  })
})


test_that("withdraw_* functions throw error for invalid input", {

  expect_error(withdraw_list(NULL), "Invalid parameter `account_uuid`")
  expect_error(withdraw_list(123), "Invalid parameter `account_uuid`")
  expect_error(withdraw_list(456L), "Invalid parameter `account_uuid`")
  expect_error(withdraw_list(7.89), "Invalid parameter `account_uuid`")
  expect_error(withdraw_list(NA), "Invalid parameter `account_uuid`")
  expect_error(withdraw_list(NaN), "Invalid parameter `account_uuid`")
  expect_error(withdraw_list("not-a-uuid"), "Invalid parameter `account_uuid`")


  expect_error(withdraw_new(NULL, 100, "USD", "d77f67ef-3456-4fd9-a020-7890fe45d0e1"),
               "Invalid parameter `account_uuid`")
  expect_error(withdraw_new(123, 100, "USD", "d77f67ef-3456-4fd9-a020-7890fe45d0e1"),
               "Invalid parameter `account_uuid`")
  expect_error(withdraw_new(NA, 100, "USD", "d77f67ef-3456-4fd9-a020-7890fe45d0e1"),
               "Invalid parameter `account_uuid`")
  expect_error(withdraw_new("not-a-uuid", 100, "USD", "d77f67ef-3456-4fd9-a020-7890fe45d0e1"),
               "Invalid parameter `account_uuid`")


  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", NULL, "USD",
                            "d77f67ef-3456-4fd9-a020-7890fe45d0e1"),
               "Invalid parameter `amount`")
  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", NA, "USD",
                            "d77f67ef-3456-4fd9-a020-7890fe45d0e1"),
               "Invalid parameter `amount`")
  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", TRUE, "USD",
                            "d77f67ef-3456-4fd9-a020-7890fe45d0e1"),
               "Invalid parameter `amount`")
  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", Inf, "USD",
                            "d77f67ef-3456-4fd9-a020-7890fe45d0e1"),
               "Invalid parameter `amount`")
  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", NaN, "USD",
                            "d77f67ef-3456-4fd9-a020-7890fe45d0e1"),
               "Invalid parameter `amount`")
  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", c(10, 20), "USD",
                            "d77f67ef-3456-4fd9-a020-7890fe45d0e1"),
               "Invalid parameter `amount`")
  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", "five", "USD",
                            "d77f67ef-3456-4fd9-a020-7890fe45d0e1"),
               "Invalid parameter `amount`")
  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", -10, "USD",
                            "d77f67ef-3456-4fd9-a020-7890fe45d0e1"),
               "Invalid parameter `amount`")

  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", 10, NULL,
                            "d77f67ef-3456-4fd9-a020-7890fe45d0e1"),
               "Invalid parameter `currency`")
  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", 10, NA,
                            "d77f67ef-3456-4fd9-a020-7890fe45d0e1"),
               "Invalid parameter `currency`")
  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", 10, FALSE,
                            "d77f67ef-3456-4fd9-a020-7890fe45d0e1"),
               "Invalid parameter `currency`")
  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", 10, 123,
                            "d77f67ef-3456-4fd9-a020-7890fe45d0e1"),
               "Invalid parameter `currency`")
  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", 10, 456L,
                            "d77f67ef-3456-4fd9-a020-7890fe45d0e1"),
               "Invalid parameter `currency`")
  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", 10, c("EUR", "USD"),
                            "d77f67ef-3456-4fd9-a020-7890fe45d0e1"),
               "Invalid parameter `currency`")
  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", 10, "ABC",
                            "d77f67ef-3456-4fd9-a020-7890fe45d0e1"),
               "Invalid parameter `currency`")

  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", 10, "USD", NULL),
               "Invalid parameter `pay_method_uuid`")
  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", 10, "USD", NA),
               "Invalid parameter `pay_method_uuid`")
  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", 10, "USD", 123),
               "Invalid parameter `pay_method_uuid`")
  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", 10, "USD", "not-a-uuid"),
               "Invalid parameter `pay_method_uuid`")
  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", 10, "USD", Inf),
               "Invalid parameter `pay_method_uuid`")
  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", 10, "USD",
                            c("d77f67ef-3456-4fd9-a020-7890fe45d0e1",
                              "d77f67ef-3456-4fd9-a020-7890fe45d0e1")),
               "Invalid parameter `pay_method_uuid`")

  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", 10, "USD",
                            "d77f67ef-3456-4fd9-a020-7890fe45d0e1", auto_confirm = "nope"),
               "Invalid parameter `auto_confirm`")
  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", 10, "USD",
                            "d77f67ef-3456-4fd9-a020-7890fe45d0e1", auto_confirm = NULL),
               "Invalid parameter `auto_confirm`")
  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", 10, "USD",
                            "d77f67ef-3456-4fd9-a020-7890fe45d0e1", auto_confirm = 123),
               "Invalid parameter `auto_confirm`")
  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", 10, "USD",
                            "d77f67ef-3456-4fd9-a020-7890fe45d0e1", auto_confirm = "TRUE"),
               "Invalid parameter `auto_confirm`")
  expect_error(withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567", 10, "USD",
                            "d77f67ef-3456-4fd9-a020-7890fe45d0e1", auto_confirm = c(TRUE, FALSE)),
               "Invalid parameter `auto_confirm`")

  expect_error(withdraw_confirm(NULL, "97c0dcf2-daf7-446c-8dcc-66a9b72b6391"),
               "Invalid parameter `account_uuid`")
  expect_error(withdraw_confirm(123, "97c0dcf2-daf7-446c-8dcc-66a9b72b6391"),
               "Invalid parameter `account_uuid`")
  expect_error(withdraw_confirm("not-a-uuid", "97c0dcf2-daf7-446c-8dcc-66a9b72b6391"),
               "Invalid parameter `account_uuid`")

  expect_error(withdraw_confirm("f2a50186-34a5-4a92-95e1-5b6cf307bb35", NULL),
               "Invalid parameter `withdraw_uuid`")
  expect_error(withdraw_confirm("f2a50186-34a5-4a92-95e1-5b6cf307bb35", 123),
               "Invalid parameter `withdraw_uuid`")
  expect_error(withdraw_confirm("f2a50186-34a5-4a92-95e1-5b6cf307bb35", "not-a-uuid"),
               "Invalid parameter `withdraw_uuid`")

})
