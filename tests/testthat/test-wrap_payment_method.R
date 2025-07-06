test_that("pay_method_list() returns a data.frame", {

  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  # We store a fake key to allow it to be read before sending the request, so that the in-memory
  # key store is populated. A real key was used to obtain the response stored (and redacted) on
  # the mock directory.
  path <- apikey_store("fake_key", openssl::ec_keygen(), "abcd")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  apikey_read("abcd")

  with_mock_api({
    res <- pay_method_list()
  })

  expect_s3_class(res, "data.frame")
  expect_true(nrow(res) > 0)
  expect_true(all(c("id", "type") %in% names(res)))
})

test_that("pay_method_get() returns a named list with expected fields", {

  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  # We store a fake key to allow it to be read before sending the request, so that the in-memory
  # key store is populated. A real key was used to obtain the response stored (and redacted) on
  # the mock directory.
  path <- apikey_store("fake_key", openssl::ec_keygen(), "abcd")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  apikey_read("abcd")

  with_mock_api({
    res <- pay_method_get("c7ff7e9a-4219-4e22-95e0-bbb1f360db6b")
  })

  expect_type(res, "list")
  expect_named(res)
  expect_true("id" %in% names(res))
  expect_true("name" %in% names(res))

})

test_that("pay_method_get() validates input UUID", {

  expect_error(pay_method_get(NULL), "Invalid parameter `method_uuid`")
  expect_error(pay_method_get(NA), "Invalid parameter `method_uuid`")
  expect_error(pay_method_get(123L), "Invalid parameter `method_uuid`")
  expect_error(pay_method_get(456), "Invalid parameter `method_uuid`")
  expect_error(pay_method_get("not-a-uuid"), "Invalid parameter `method_uuid`")

})
