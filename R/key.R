#' API Key - Store
#'
#' Stores the API key securely for latter retrieval.
#' By default it will not overwrite a previously existing key unless explicitly set.
#'
#' @param key_name  String. The key name.
#' @param key       String. The API key.
#' @param password  String. A password to encrypt the key for storage.
#' @param overwrite Boolean. What to do if a previous key file is detected and function is called in
#'                  non-interactive session.
#'
#' @returns Silently, the key file path if successful.
#' @export
#'
#' @examples
#'
#' apikey_store("my_key_name",
#'              "my_secret_api_key",
#'              "a_very_strong_password_that_nobody_will_ever_find")
#'
apikey_store <- function(key_name, key, password = NULL, overwrite = FALSE) {

  key_obj <- serialize(list(name = key_name,
                            key = key),
                       NULL)

  salt <- openssl::rand_bytes(16)
  if (is.null(password)) {
    if (interactive()) {
      pass_prompt <- "Please enter a password for secure API key storage: "
      password <- openssl::askpass(pass_prompt)
      if (is.null(password)) {
        stop("Cancelled by user. Aborting...")
      }

      pass_prompt <- "Please re-enter the password: "
      password_verify <- openssl::askpass(pass_prompt)
      if (is.null(password_verify)) {
        rm(password)
        gc(verbose = FALSE)
        stop("Cancelled by user. Aborting...")
      }

      if (password != password_verify) {
        rm(password, password_verify)
        gc(verbose = FALSE)
        stop("Entered passwords did not match. Aborting...")
      }

      rm(password_verify)
      gc(verbose = FALSE)
    } else {
      suppressWarnings(rm(salt, key_obj))
      gc(verbose = FALSE)
      stop("Password not provided on non-interactive session. Aborting...")
    }
  }

  tryCatch({
    db_key <- openssl::bcrypt_pbkdf(password, salt, 64L, 32L)
  },
  error = function(e) {
    suppressWarnings(rm(key_obj, salt, password))
    gc(verbose = FALSE)
    stop("Failure while deriving encryption key from the password: ", e$message)
  })
  rm(password)

  tryCatch({
    key_obj_enc <- openssl::aes_gcm_encrypt(key_obj, db_key)
  }, error = function(e) {
    suppressWarnings(rm(key_obj, salt, db_key))
    gc(verbose = FALSE)
    stop("Failure while encrypting the keys: ", e$message)
  })


  key_path <- rappdirs::user_config_dir("coinbaseapi")
  key_filename <- paste0(key_path, "api.key")
  writeBin(key_obj_enc, key_filename)

}
