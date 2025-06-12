
test_that("API key can be stored and read successfully", {
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  key_name <- "test_key"
  key_value <- openssl::ec_keygen()
  password <- "super_strong_pass"

  path <- apikey_store(key_name, key_value, password)
  on.exit(unlink(path, force = TRUE), add = TRUE)

  expect_true(file.exists(path))

  result <- apikey_read(password)

  expect_equal(result, TRUE)
  expect_equal(.rcoinbaseapi_key_mem_store$name, key_name)
  expect_equal(.rcoinbaseapi_key_mem_store$key, key_value)
})


test_that("Key reading with wrong password fails", {
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  path <- apikey_store("name", "value", "correct_password")
  on.exit(unlink(path, force = TRUE), add = TRUE)

  expect_error(apikey_read("wrong_password"), "Failed to decrypt API key")
})


test_that("Storing without password in non-interactive session fails", {
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  expect_error(apikey_store("x", "y"), "Password must be provided in non-interactive sessions")
})

test_that("Key reading without password in non-interactive session fails", {
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  path <- apikey_store("name", "value", "correct_password")
  on.exit(unlink(path, force = TRUE), add = TRUE)

  # Simulate non-interactive session by forcing password = NULL
  expect_error(apikey_read(), "Password must be provided in non-interactive sessions")
})


test_that("Storing twice without overwrite in non-interactive session does not overwrite file", {
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  key_name <- "test_key"
  key_value_1 <- openssl::ec_keygen()
  key_value_2 <- openssl::ec_keygen()
  password <- "password123"

  path <- apikey_store(key_name, key_value_1, password)
  on.exit(unlink(path, force = TRUE), add = TRUE)

  # Second attempt with different value should not overwrite
  expect_error(apikey_store(key_name, key_value_2, password, overwrite = FALSE))

  apikey_read(password)
  expect_equal(.rcoinbaseapi_key_mem_store$key, key_value_1)
})


test_that("Storing twice with overwrite in non-interactive session overwrites file", {
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  key_name <- "test_key"
  key_value_1 <- openssl::ec_keygen()
  key_value_2 <- openssl::ec_keygen()
  password <- "password123"

  path <- apikey_store(key_name, key_value_1, password)
  on.exit(unlink(path, force = TRUE), add = TRUE)

  # Second attempt with different value should overwrite
  apikey_store(key_name, key_value_2, password, overwrite = TRUE)

  apikey_read(password)
  expect_equal(.rcoinbaseapi_key_mem_store$key, key_value_2)
})


test_that("Key reading fails if no key file is present", {
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  path <- file.path(tempdir(), "api.key")
  if (file.exists(path)) file.remove(path)

  expect_error(apikey_read("some_password"), "No API key found")
})


test_that("Key reading fails if file is corrupted", {
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  file_path <- file.path(tempdir(), "api.key")
  writeLines("not a valid binary file", file_path)
  on.exit(unlink(file_path, force = TRUE), add = TRUE)

  expect_error(apikey_read("some_password"), "Failed to parse stored key file")
})


test_that("Key reading fails if key file is missing required fields", {
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  file_path <- file.path(tempdir(), "api.key")
  fake_data <- serialize(list(salt = "xyz", encrypted_key = "abc"), NULL)
  writeBin(fake_data, file_path)
  on.exit(unlink(file_path, force = TRUE), add = TRUE)

  expect_error(apikey_read("some_password"), "Invalid key file format")
})


test_that("Key reading fails if memory store not available", {
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  path <- apikey_store("name", openssl::ec_keygen(), password = "pass")
  on.exit(unlink(path, force = TRUE), add = TRUE)

  # Simulate missing target environment
  local_mocked_bindings(exists = function(x, where, envir, frame, mode, inherits) FALSE,
                        .package = "base")
  expect_error(apikey_read("pass"), "Unable to store key information")
})


test_that("Interactive password prompt success path works when writing file", {
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  local_mocked_bindings(interactive = function() TRUE)
  local_mocked_bindings(askpass = function(prompt) {
    if (grepl("re-enter", prompt)) return("match")
    return("match")},
    .package = "openssl")

  key <- openssl::ec_keygen()
  path <- apikey_store("name", key, , password = NULL)
  on.exit(unlink(path, force = TRUE), add = TRUE)

  result <- apikey_read("match")
  expect_equal(result, TRUE)
  expect_equal(.rcoinbaseapi_key_mem_store$name, "name")
  expect_equal(.rcoinbaseapi_key_mem_store$key, key)
})


test_that("Interactive password prompt cancel triggers abort when writing the key", {
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")
  local_mocked_bindings(interactive = function() TRUE)
  local_mocked_bindings(askpass = function(prompt) NULL, .package = "openssl")

  expect_error(
    apikey_store("x", "y", password = NULL),
    "Cancelled by user"
  )
})


test_that("Interactive password confirmation prompt cancel triggers abort", {
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")
  local_mocked_bindings(interactive = function() TRUE)
  local_mocked_bindings(askpass = function(prompt) {
    if (grepl("re-enter", prompt)) return(NULL)
    return("pass")},
    .package = "openssl")

  expect_error(
    apikey_store("x", "y", password = NULL),
    "Cancelled by user"
  )
})


test_that("Interactive password prompt mismatch triggers abort", {
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")
  local_mocked_bindings(interactive = function() TRUE)
  local_mocked_bindings(askpass = function(prompt) {
    if (grepl("re-enter", prompt)) return("abc")
    return("def")},
    .package = "openssl")

  expect_error(apikey_store("x", "y", password = NULL),
               "Entered passwords not matching")
})


test_that("Interactive overwrite prompt triggers cancel", {
  overwrite_called <- FALSE
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")
  local_mocked_bindings(interactive = function() TRUE)
  local_mocked_bindings(menu = function(...) {
    overwrite_called <<- TRUE
    return(1)  # Cancel
  },
  .package = "utils"
  )

  path <- apikey_store("foo", "bar", "pw")
  on.exit(unlink(path, force = TRUE), add = TRUE)

  expect_error(apikey_store("foo", "bar", "pw"), "Cancelled by user")
  expect_true(overwrite_called)
})


test_that("Interactive password prompt success path works when reading file", {
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  key <- openssl::ec_keygen()
  path <- apikey_store("name", key, "pass", TRUE)
  on.exit(unlink(path, force = TRUE), add = TRUE)

  local_mocked_bindings(interactive = function() TRUE)
  local_mocked_bindings(askpass = function(prompt) return("pass"), .package = "openssl")

  result <- apikey_read()
  expect_equal(result, TRUE)
  expect_equal(.rcoinbaseapi_key_mem_store$name, "name")
  expect_equal(.rcoinbaseapi_key_mem_store$key, key)
})


test_that("Interactive password prompt cancel triggers abort when reading key", {
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")
  path <- apikey_store("name", "value", password = "pass")
  on.exit(unlink(path, force = TRUE), add = TRUE)

  local_mocked_bindings(interactive = function() TRUE)
  local_mocked_bindings(askpass = function(prompt) NULL, .package = "openssl")

  expect_error(apikey_read(), "Cancelled by user")
})


test_that("Fails gracefully on bcrypt_pbkdf() error when writing key", {
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")
  local_mocked_bindings(bcrypt_pbkdf = function(...) stop("bcrypt failed"), .package = "openssl")

  expect_error(apikey_store("a", "b", password = "x"),
               "Failure deriving encryption key")
})


test_that("Fails gracefully on bcrypt_pbkdf() error when reading key", {
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")

  path <- apikey_store("a", "b", password = "x")
  on.exit(unlink(path, force = TRUE), add = TRUE)

  local_mocked_bindings(bcrypt_pbkdf = function(...) stop("bcrypt failed"), .package = "openssl")
  expect_error(apikey_read("x"), "Failure deriving decryption")

})


test_that("Fails gracefully on aes_gcm_encrypt() error", {
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")
  local_mocked_bindings(aes_gcm_encrypt = function(...) stop("encryption failed"),
                        .package = "openssl")

  expect_error(apikey_store("a", "b", password = "x"),
               "Failure encrypting the key")
})


test_that("Fails gracefully on aes_gcm_decrypt() error", {
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")
  local_mocked_bindings(aes_gcm_decrypt = function(...) stop("decryption failed"),
                        .package = "openssl")

  path <- apikey_store("foo", "bar", "pw")
  on.exit(unlink(path, force = TRUE), add = TRUE)

  expect_error(apikey_read("pw"), "Failed to decrypt API key")
})


test_that("Fails gracefully on writeBin() error", {
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")
  local_mocked_bindings(writeBin = function(...) stop("file save failed"), .package = "base")

  expect_error(apikey_store("foo", "bar", "pw"),
               "file save failed")
})


test_that("Fails gracefully on readBin() error", {
  local_mocked_bindings(user_config_dir = function(x) tempdir(), .package = "rappdirs")
  local_mocked_bindings(readBin = function(...) stop("file read failed"), .package = "base")

  path <- apikey_store("foo", "bar", "pw")
  on.exit(unlink(path, force = TRUE), add = TRUE)

  expect_error(apikey_read("pw"),
               "Failure reading the stored key file")
})
