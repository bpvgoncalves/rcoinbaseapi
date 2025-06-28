
test_that("check_clock_drift() returns correct class and fields", {

  skip_if_offline()
  result <- check_clock_drift(threshold = 99999)  # Ensure no warning triggers

  expect_s3_class(result, "rcoinbaseapi_clock_drift")
  expect_named(result, c("local", "server", "time_delta"))
  expect_type(result$local, "integer")
  expect_type(result$server, "integer")
  expect_type(result$time_delta, "integer")
})

test_that("check_clock_drift() detects large drift", {
  # Mock scenario: pretend local time is far off
  with_mocked_bindings(
    server_time = function() list(epoch = as.integer(Sys.time()) - 1000L),
    {
      expect_message(
        result <- check_clock_drift(threshold = 30),
        regexp = "System clock differs from Coinbase server"
      )
      expect_s3_class(result, "rcoinbaseapi_clock_drift")
      expect_gt(abs(result$time_delta), 30)
    }
  )
})
