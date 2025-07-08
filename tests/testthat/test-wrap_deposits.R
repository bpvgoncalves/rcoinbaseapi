test_that("deposit_list() returns data with valid account_uuid", {

  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  # We store a fake key to allow it to be read before sending the request, so that the in-memory
  # key store is populated. A real key was used to obtain the response stored (and redacted) on
  # the mock directory.
  path <- apikey_store("fake_key", openssl::ec_keygen(), "abcd")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  apikey_read("abcd")

  with_mock_api(result <- deposit_list("f2a50186-34a5-4a92-95e1-5b6cf307bb35"))

  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_named(result,
               c("id", "status", "user_reference", "created_at", "updated_at", "resource",
                 "resource_path", "commited", "payout_at", "instant", "hold_until", "hold_days",
                 "fee_explanation_url", "idem", "transaction.transaction_id",
                 "transaction.resource", "transaction.resource_path", "payment_method.id",
                 "payment_method.resource", "payment_method.resource_path", "amount.amount",
                 "amount.currency", "subtotal.amount", "subtotal.currency", "cancel_reason.id",
                 "cancel_reason.message", "fee.amount", "fee.currency"),
               ignore.order = TRUE)
})


test_that("deposit_new() returns deposit confirmation with correct inputs", {

  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  # We store a fake key to allow it to be read before sending the request, so that the in-memory
  # key store is populated. A real key was used to obtain the response stored (and redacted) on
  # the mock directory.
  path <- apikey_store("fake_key", openssl::ec_keygen(), "abcd")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  apikey_read("abcd")

  with_mock_api(
    result <- deposit_new(account_uuid = "f2a50186-34a5-4a92-95e1-5b6cf307bb35",
                          amount = 100,
                          currency = "USD",
                          pay_method_uuid = "c7ff7e9a-4219-4e22-95e0-bbb1f360db6b",
                          auto_confirm = FALSE))

  expect_type(result, "list")
  expect_true("id" %in% names(result))
})


test_that("deposit_confirm() returns confirmation for deposit UUID", {

  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  # We store a fake key to allow it to be read before sending the request, so that the in-memory
  # key store is populated. A real key was used to obtain the response stored (and redacted) on
  # the mock directory.
  path <- apikey_store("fake_key", openssl::ec_keygen(), "abcd")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  apikey_read("abcd")

  with_mock_api(result <- deposit_confirm(account_uuid = "f2a50186-34a5-4a92-95e1-5b6cf307bb35",
                                          deposit_uuid = "fb8e12e7-d3df-4d9c-b508-126b72c7b099"))

  expect_type(result, "list")
  expect_true("status" %in% names(result))
})


test_that("deposit functions throw error for invalid input", {

  expect_error(deposit_list(NULL), "Invalid parameter `account_uuid`")
  expect_error(deposit_list(123), "Invalid parameter `account_uuid`")
  expect_error(deposit_list(456L), "Invalid parameter `account_uuid`")
  expect_error(deposit_list(NA), "Invalid parameter `account_uuid`")
  expect_error(deposit_list(NaN), "Invalid parameter `account_uuid`")
  expect_error(deposit_list("not-a-uuid"), "Invalid parameter `account_uuid`")

  expect_error(deposit_new(NULL, 100, "USD", "ce2316d0-29d6-4bf5-85c5-940dfe19acbb"),
               "Invalid parameter `account_uuid`")
  expect_error(deposit_new(123, 100, "USD", "ce2316d0-29d6-4bf5-85c5-940dfe19acbb"),
               "Invalid parameter `account_uuid`")
  expect_error(deposit_new("not-a-uuid", 100, "USD", "ce2316d0-29d6-4bf5-85c5-940dfe19acbb"),
               "Invalid parameter `account_uuid`")

  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2",
                           NULL,
                           "USD",
                           "ce2316d0-29d6-4bf5-85c5-940dfe19acbb"),
               "Invalid parameter `amount`")
  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2", NA, "USD",
                           "ce2316d0-29d6-4bf5-85c5-940dfe19acbb"),
               "Invalid parameter `amount`")
  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2", TRUE, "USD",
                           "ce2316d0-29d6-4bf5-85c5-940dfe19acbb"),
               "Invalid parameter `amount`")
  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2", Inf, "USD",
                           "ce2316d0-29d6-4bf5-85c5-940dfe19acbb"),
               "Invalid parameter `amount`")
  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2", c(1.11, 2.22), "USD",
                           "ce2316d0-29d6-4bf5-85c5-940dfe19acbb"),
               "Invalid parameter `amount`")
  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2", "not-numeric", "USD",
                           "ce2316d0-29d6-4bf5-85c5-940dfe19acbb"),
               "Invalid parameter `amount`")
  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2", -1.23, "USD",
                           "ce2316d0-29d6-4bf5-85c5-940dfe19acbb"),
               "Invalid parameter `amount`")

  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2", 100, NULL,
                           "ce2316d0-29d6-4bf5-85c5-940dfe19acbb"),
               "Invalid parameter `currency`")
  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2", 100, NA,
                           "ce2316d0-29d6-4bf5-85c5-940dfe19acbb"),
               "Invalid parameter `currency`")
  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2", 100, FALSE,
                           "ce2316d0-29d6-4bf5-85c5-940dfe19acbb"),
               "Invalid parameter `currency`")
  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2", 100, 123,
                           "ce2316d0-29d6-4bf5-85c5-940dfe19acbb"),
               "Invalid parameter `currency`")
  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2", 100, 456L,
                           "ce2316d0-29d6-4bf5-85c5-940dfe19acbb"),
               "Invalid parameter `currency`")
  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2", 100, c("EUR", "USD"),
                           "ce2316d0-29d6-4bf5-85c5-940dfe19acbb"),
               "Invalid parameter `currency`")

  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2", 100, "USD", NULL),
               "Invalid parameter `pay_method_uuid`")
  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2", 100, "USD", NA),
               "Invalid parameter `pay_method_uuid`")
  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2", 100, "USD", 123),
               "Invalid parameter `pay_method_uuid`")
  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2", 100, "USD", "not-a-uuid"),
               "Invalid parameter `pay_method_uuid`")
  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2", 100, "USD", Inf),
               "Invalid parameter `pay_method_uuid`")
  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2",
                           100,
                           "USD",
                           c("ce2316d0-29d6-4bf5-85c5-940dfe19acbb",
                             "ce2316d0-29d6-4bf5-85c5-940dfe19acbb")),
               "Invalid parameter `pay_method_uuid`")

  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2", 100, "USD",
                           "ce2316d0-29d6-4bf5-85c5-940dfe19acbb", auto_confirm = "yes"),
               "Invalid parameter `auto_confirm`")
  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2", 100, "USD",
                           "ce2316d0-29d6-4bf5-85c5-940dfe19acbb", auto_confirm = NULL),
               "Invalid parameter `auto_confirm`")
  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2", 100, "USD",
                           "ce2316d0-29d6-4bf5-85c5-940dfe19acbb", auto_confirm = 123),
               "Invalid parameter `auto_confirm`")
  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2", 100, "USD",
                           "ce2316d0-29d6-4bf5-85c5-940dfe19acbb", auto_confirm = "TRUE"),
               "Invalid parameter `auto_confirm`")
  expect_error(deposit_new("b5d211c3-95fe-451b-95e2-6f97442655b2", 100, "USD",
                           "ce2316d0-29d6-4bf5-85c5-940dfe19acbb", auto_confirm = c(TRUE, FALSE)),
               "Invalid parameter `auto_confirm`")

  expect_error(deposit_confirm(NULL, "0d08e9b8-8859-4f29-bd1e-c7b6cfbe88a5"),
               "Invalid parameter `account_uuid`")
  expect_error(deposit_confirm(123, "0d08e9b8-8859-4f29-bd1e-c7b6cfbe88a5"),
               "Invalid parameter `account_uuid`")
  expect_error(deposit_confirm("not-a-uuid", "0d08e9b8-8859-4f29-bd1e-c7b6cfbe88a5"),
               "Invalid parameter `account_uuid`")

  expect_error(deposit_confirm("b5d211c3-95fe-451b-95e2-6f97442655b2", NULL),
               "Invalid parameter `deposit_uuid`")
  expect_error(deposit_confirm("b5d211c3-95fe-451b-95e2-6f97442655b2", 123),
               "Invalid parameter `deposit_uuid`")
  expect_error(deposit_confirm("b5d211c3-95fe-451b-95e2-6f97442655b2", "not-a-uuid"),
               "Invalid parameter `deposit_uuid`")
})
