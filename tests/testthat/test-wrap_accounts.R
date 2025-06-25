test_that("'account_list' wrapper works", {

  skip_if_offline()

  accs <- account_list(use_sandbox = TRUE)

  expect_s3_class(accs, "data.frame")
  expect_equal(nrow(accs), 4L)
  expect_equal(ncol(accs), 16L)
  expect_equal(accs[1, 3], "USDC")
  expect_equal(accs[1, 13], "100")

})


test_that("'account_list' throws error on bad parameters", {

  skip_if_offline()

  expect_error(account_list(limit = NA, use_sandbox = TRUE))
  expect_error(account_list(limit = "abc", use_sandbox = TRUE))
  expect_error(account_list(limit = c(1, 2), use_sandbox = TRUE))

  expect_error(account_list(cursor = NA, use_sandbox = TRUE))
  expect_error(account_list(cursor = 123, use_sandbox = TRUE))
  expect_error(account_list(cursor = c("a", "b"), use_sandbox = TRUE))

})
