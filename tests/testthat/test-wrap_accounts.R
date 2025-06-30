
test_that("'account_list' wrapper works", {

  with_mock_api(accs <- account_list(use_sandbox = TRUE))

  expect_s3_class(accs, "data.frame")
  expect_equal(nrow(accs), 4L)
  expect_equal(ncol(accs), 16L)
  expect_equal(accs[1, 3], "USDC")
  expect_equal(accs[1, 13], "100")

  accs2 <- account_list(limit = 2L, use_sandbox = TRUE)
  expect_identical(accs, accs2)

  expect_message(accs3 <- account_list(limit = 2000L, use_sandbox = TRUE),
                 "exceeds maximum of 250")
  expect_identical(accs, accs3)

  accs4 <- account_list(cursor = "abc", use_sandbox = TRUE)
  expect_identical(accs, accs4)

})


test_that("'account_list' throws error on bad parameters", {

  expect_error(account_list(limit = NA, use_sandbox = TRUE), "Invalid parameter `limit`")
  expect_error(account_list(limit = "abc", use_sandbox = TRUE), "Invalid parameter `limit`")
  expect_error(account_list(limit = c(1, 2), use_sandbox = TRUE), "Invalid parameter `limit`")

  expect_error(account_list(cursor = NA, use_sandbox = TRUE), "Invalid parameter `cursor`")
  expect_error(account_list(cursor = 123, use_sandbox = TRUE), "Invalid parameter `cursor`")
  expect_error(account_list(cursor = c("a", "b"), use_sandbox = TRUE), "Invalid parameter `cursor`")

  expect_error(account_list(use_sandbox = NULL), "Invalid parameter `use_sandbox`")
  expect_error(account_list(use_sandbox = NA), "Invalid parameter `use_sandbox`")
  expect_error(account_list(use_sandbox = 123), "Invalid parameter `use_sandbox`")
  expect_error(account_list(use_sandbox = "abc"), "Invalid parameter `use_sandbox`")
  expect_error(account_list(use_sandbox = c(TRUE, TRUE)), "Invalid parameter `use_sandbox`")

})


test_that("'account_get' wrapper works", {

  acc <- account_get("66f975a6-bb2e-44be-82e9-cd8669e404b0", use_sandbox = TRUE)

  expect_true(inherits(acc, "list"))
  expect_equal(length(acc), 14L)
  expect_equal(acc$name, "USDC Wallet")
  expect_equal(acc$currency, "USDC")

})


test_that("'account_get' throws error on bad parameters", {

  expect_error(account_get(use_sandbox = TRUE),
               "Invalid parameter `account_uuid`")
  expect_error(account_get(12345, use_sandbox = TRUE),
               "Invalid parameter `account_uuid`")
  expect_error(account_get("abcd", use_sandbox = TRUE),
               "Invalid parameter `account_uuid`")
  expect_error(account_get(c("66f975a6-bb2e-44be-82e9-cd8669e404b0",
                             "00000000-bbbb-44be-82e9-ffffffffffff"),
                           use_sandbox = TRUE),
               "Invalid parameter `account_uuid`")
  expect_error(account_get("66f975a6-bb2e-44be-82e9-cd8669e404bg", use_sandbox = TRUE),
               "Invalid parameter `account_uuid`")

  expect_error(account_get("66f975a6-bb2e-44be-82e9-cd8669e404b0", use_sandbox = NULL),
               "Invalid parameter `use_sandbox`")
  expect_error(account_get("66f975a6-bb2e-44be-82e9-cd8669e404b0", use_sandbox = NA),
               "Invalid parameter `use_sandbox`")
  expect_error(account_get("66f975a6-bb2e-44be-82e9-cd8669e404b0", use_sandbox = 123),
               "Invalid parameter `use_sandbox`")
  expect_error(account_get("66f975a6-bb2e-44be-82e9-cd8669e404b0", use_sandbox = "abc"),
               "Invalid parameter `use_sandbox`")
  expect_error(account_get("66f975a6-bb2e-44be-82e9-cd8669e404b0", use_sandbox = c(TRUE, TRUE)),
               "Invalid parameter `use_sandbox`")

})
