
test_that("server_time() returns expected structure", {

  with_mock_api(result <- server_time())

  expect_type(result, "list")
  expect_named(result, c("iso", "epochSeconds", "epochMillis"), ignore.order = TRUE)
  expect_match(result$iso, "^\\d{4}-\\d{2}-\\d{2}T")
  expect_type(result$epochSeconds, "double")
  expect_type(result$epochMillis, "double")

})
