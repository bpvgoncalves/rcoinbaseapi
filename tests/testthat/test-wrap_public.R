
test_that("server_time() returns expected structure", {

  with_mock_api(result <- server_time())

  expect_type(result, "list")
  expect_named(result, c("iso", "epochSeconds", "epochMillis"), ignore.order = TRUE)
  expect_match(result$iso, "^\\d{4}-\\d{2}-\\d{2}T")
  expect_type(result$epochSeconds, "double")
  expect_type(result$epochMillis, "double")

})


test_that("currencies() returns expected data.frame", {

  ccys <- currencies()

  expect_s3_class(ccys, "data.frame")
  expect_equal(dim(ccys), c(176, 3))
  expect_equal(ccys[1, 1], "AED")
  expect_equal(ccys[45, 2], "Euro")

})


test_that("cryptocurrencies() returns expected data.frame", {

  cryp <- cryptocurrencies()

  expect_s3_class(cryp, "data.frame")
  expect_equal(dim(cryp), c(307, 10))
  expect_equal(cryp[1, 2], "BTC")
  expect_equal(cryp[2, 3], "Ethereum")

})
