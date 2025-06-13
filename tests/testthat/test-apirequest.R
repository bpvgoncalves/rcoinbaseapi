
with_mock_api({
  test_that("API call without authorization is successful", {

    resp <- apirequest("GET", "v2/time", FALSE)

    expect_s3_class(resp, "httr2_response")
    expect_equal(resp$method, "GET")
    expect_equal(resp$status_code, 200L)

  })

  test_that("API call with authorization is successful", {

    local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

    # We store a fake key to allow it to be read before sending the request, so that the in-memory
    # key store is populated. A real key was used to obtain the response stored (and redacted) on
    # the mock directory.
    apikey_store("fake_key", openssl::ec_keygen(), "abcd")
    apikey_read("abcd")
    resp <- apirequest("GET", "api/v3/brokerage/portfolios", TRUE)

    expect_s3_class(resp, "httr2_response")
    expect_equal(resp$method, "GET")
    expect_equal(resp$status_code, 200L)

  })

  test_that("Authorization fails when key vault is missing", {

    local_mocked_bindings(exists = function(x, envir, mode, inherits) FALSE, .package = "base")
    expect_error(resp <- apirequest("GET", "api/v3/brokerage/portfolios", TRUE),
                 "Cannot find in-memory key vault")

  })

  test_that("Authorization fails when key is not loaded", {

    local_mocked_bindings(all = function(...) FALSE, .package = "base")
    expect_error(resp <- apirequest("GET", "api/v3/brokerage/portfolios", TRUE),
                 "Key information not loaded")

  })
})
