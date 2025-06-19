
with_mock_api({
  test_that("API call without authorization is successful", {

    resp <- apirequest("GET", "v2/time", need_auth = FALSE)

    expect_s3_class(resp, "httr2_response")
    expect_equal(resp$method, "GET")
    expect_equal(resp$status_code, 200L)

  })

  test_that("Request successfull with right parameters", {

    resp <- apirequest("GET",
                       "api/v3/brokerage/market/products",
                       path_params = "BTC-USD",
                       need_auth = FALSE)
    expect_s3_class(resp, "httr2_response")

    resp <- apirequest("GET",
                       "",
                       path_params = c("api/v3/brokerage/market/products", "BTC-USD"),
                       need_auth = FALSE)
    expect_s3_class(resp, "httr2_response")

    resp <- apirequest("GET",
                       "api/v3/brokerage/market/product_book",
                       query_params = list(product_id = "BTC-USD", limit = 1L),
                       need_auth = FALSE)
    expect_s3_class(resp, "httr2_response")

  })

  test_that("API call with authorization is successful", {

    local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

    # We store a fake key to allow it to be read before sending the request, so that the in-memory
    # key store is populated. A real key was used to obtain the response stored (and redacted) on
    # the mock directory.
    path <- apikey_store("fake_key", openssl::ec_keygen(), "abcd")
    on.exit(unlink(path, force = TRUE), add = TRUE)
    apikey_read("abcd")
    resp <- apirequest("GET", "api/v3/brokerage/portfolios", need_auth = TRUE)

    expect_s3_class(resp, "httr2_response")
    expect_equal(resp$method, "GET")
    expect_equal(resp$status_code, 200L)

  })

  test_that("Authorization fails when key vault is missing", {

    local_mocked_bindings(exists = function(x, envir, mode, inherits) FALSE, .package = "base")
    expect_error(resp <- apirequest("GET", "api/v3/brokerage/portfolios", need_auth = TRUE),
                 "Cannot find in-memory key vault")

  })

  test_that("Authorization fails when key is not loaded", {

    local_mocked_bindings(all = function(...) FALSE, .package = "base")
    expect_error(resp <- apirequest("GET", "api/v3/brokerage/portfolios", need_auth = TRUE),
                 "Signing key information not loaded or already expired")

  })

  test_that("Fails on no internet", {
    without_internet({
      expect_error(apirequest("GET", "v2/time", need_auth = FALSE),
                   "Failure executing the request")
    })
  })
})


test_that("Request fails on invalid method", {

  expect_error(apirequest("TEST", "v2/time", need_auth = FALSE),
               "Invalid parameter `method`")

})


test_that("Request fails on invalid endpoint", {

  expect_error(apirequest("GET", need_auth = FALSE),
               "Invalid parameter `endpoint`")

  expect_error(apirequest("GET", NA, need_auth = FALSE),
               "Invalid parameter `endpoint`")

  expect_error(apirequest("GET", c("v2/time", "v99/test"), need_auth = FALSE),
               "Invalid parameter `endpoint`")

})


test_that("Request fails on invalid path_parameters", {

  expect_error(apirequest("GET", "v2/time", NA, need_auth = FALSE),
               "Invalid parameter `path_params`")

  expect_error(apirequest("GET", "v2/time", Inf, need_auth = FALSE),
               "Invalid parameter `path_params`")

})


test_that("Request fails on invalid query_parameters", {

  expect_error(apirequest("GET", "v2/time", query_params = "abc", need_auth = FALSE),
               "Invalid parameter `query_params`")

  expect_error(apirequest("GET", "v2/time", query_params = NA, need_auth = FALSE),
               "Invalid parameter `query_params`")

  expect_error(apirequest("GET", "v2/time", query_params = list("a" = 1, 2), need_auth = FALSE),
               "Invalid parameter `query_params`")

})


test_that("Request fails on invalid body_parameters", {

  expect_error(apirequest("GET", "v2/time", body_params = "abc", need_auth = FALSE),
               "Invalid parameter `body_params`")

  expect_error(apirequest("GET", "v2/time", body_params = NA, need_auth = FALSE),
               "Invalid parameter `body_params`")

  expect_error(apirequest("GET", "v2/time", body_params = list("a" = 1, 2), need_auth = FALSE),
               "Invalid parameter `body_params`")

})

test_that("Request fails on invalid need_auth parameter", {

  expect_error(apirequest("GET", "v2/time", need_auth = NULL),
               "Invalid parameter `need_auth`")

  expect_error(apirequest("GET", "v2/time", need_auth = NA),
               "Invalid parameter `need_auth`")

  expect_error(apirequest("GET", "v2/time", need_auth = "abc"),
               "Invalid parameter `need_auth`")

})
