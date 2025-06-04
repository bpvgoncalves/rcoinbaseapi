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

  if (is.null(password)) {
    db_key <- openssl::sha3(openssl::askpass())
  } else {
    db_key <- openssl::sha3(password)
    rm(password)
  }

  key_obj_enc <- openssl::aes_gcm_encrypt(key_obj, db_key)


  key_path <- rappdirs::user_config_dir("coinbaseapi")
  key_filename <- paste0(key_path, "api.key")
  writeBin(key_obj_enc, key_filename)

}
