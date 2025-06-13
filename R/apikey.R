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
#' \dontrun{
#' # WARNING: running this example may result in a previously stored key to be overwritten.
#' apikey_store("my_key_name",
#'              openssl::ec_keygen(),
#'              "a_very_strong_password_that_nobody_will_ever_find")
#' }
#'
apikey_store <- function(key_name, key, password = NULL, overwrite = FALSE) {

  key_obj <- serialize(list(name = key_name,
                            key = key),
                       NULL)
  rm(key)

  salt <- openssl::rand_bytes(16)
  if (is.null(password)) {
    if (interactive()) {
      pass_prompt <- "Please enter a password for secure API key storage: "
      password <- openssl::askpass(pass_prompt)
      if (is.null(password)) {
        cli::cli_abort(c("Cancelled by user.", "x" = "Aborting..."))
      }

      pass_prompt <- "Please re-enter the password: "
      password_verify <- openssl::askpass(pass_prompt)
      if (is.null(password_verify)) {
        rm(password)
        gc(verbose = FALSE)
        cli::cli_abort(c("Cancelled by user.", "x" = "Aborting..."))
      }

      if (password != password_verify) {
        rm(password, password_verify)
        gc(verbose = FALSE)
        cli::cli_abort(c("Entered passwords not matching.", "x" = "Aborting..."))
      }
      rm(password_verify)
      gc(verbose = FALSE)
    } else {
      suppressWarnings(rm(salt, key_obj))
      gc(verbose = FALSE)
      cli::cli_abort(c("Password must be provided in non-interactive sessions.",
                       "x" = "Aborting..."))
    }
  }

  tryCatch({
    db_key <- openssl::bcrypt_pbkdf(password, salt, 64L, 32L)
  },
  error = function(e) {
    suppressWarnings(rm(key_obj, salt, password))
    gc(verbose = FALSE)
    cli::cli_abort(c("Failure deriving encryption key from the password.",
                     "Original error message: ",
                     e$message))
  })
  rm(password)

  tryCatch({
    key_obj_enc <- openssl::aes_gcm_encrypt(key_obj, db_key)
  }, error = function(e) {
    suppressWarnings(rm(key_obj, salt, db_key))
    gc(verbose = FALSE)
    cli::cli_abort(c("Failure encrypting the key.",
                     "Original error message: ",
                     e$message))
  })
  rm(key_obj, db_key)


  key_path <- rappdirs::user_config_dir(utils::packageName())
  key_filename <- file.path(key_path, "api.key")
  proceed <- TRUE
  if (file.exists(key_filename)) {
    if (interactive()) {
      proceed <- switch(utils::menu(c("No", "Yes"),
                                    TRUE,
                                    "File already exists. Overwrite?"),
                        "1" = FALSE,
                        "2" = TRUE)
    } else {
      proceed <- overwrite
    }
  }
  if (proceed) {
    if (!dir.exists(key_path)) {
      dir.create(key_path, recursive = TRUE, mode = "0700")
    }
    tryCatch({
      writeBin(serialize(list(version = 1L,
                              salt = salt,
                              encrypted_key = key_obj_enc),
                         NULL),
               key_filename)
    },
    error = function(e) {
      suppressWarnings(rm(salt, key_obj_enc))
      gc(verbose = FALSE)
      cli::cli_abort(c("Failure saving the file.",
                       "Original error message: ",
                       e$message))
    })

    Sys.chmod(key_path, "0700")
    Sys.chmod(key_filename, "600")

  } else {
    suppressWarnings(rm(salt, key_obj_enc))
    gc(verbose = FALSE)
    cli::cli_abort(c("Cancelled by user.", "x" = "Aborting..."))
  }

  suppressWarnings(rm(salt, key_obj_enc))
  gc(verbose = FALSE)

  return(invisible(key_filename))
}


#' API Key - Read
#'
#' Reads and decrypts a previously stored API key.
#'
#' @param password String. The password used to encrypt the key.
#' @param timeout  Integer (default = 60s). Time, in seconds, to keep the keys loaded in memory. If
#' 0 (zero) is provided this protection will not be active.
#'
#' @returns Stores key information in memory and returns TRUE if successful.
#' @export
#'
#' @examples
#' \dontrun{
#' apikey_read("a_very_strong_password_that_nobody_will_ever_find")
#' }
#'
apikey_read <- function(password = NULL, timeout = 60L) {

  key_path <- rappdirs::user_config_dir(utils::packageName())
  key_filename <- file.path(key_path, "api.key")

  if (!file.exists(key_filename)) {
    rm(password)
    gc(verbose = FALSE)
    cli::cli_abort(c("No API key found at the expected location.", "x" = "Aborting..."))
  }

  tryCatch({
    key_bin <- readBin(key_filename, what = "raw", n = file.info(key_filename)$size)
  },
  error = function(e) {
    suppressWarnings(rm(password))
    gc(verbose = FALSE)
    cli::cli_abort(c("Failure reading the stored key file.",
                     "Original error message: ",
                     e$message))
  })

  tryCatch({
    key_data <- unserialize(key_bin)
  }, error = function(e) {
    suppressWarnings(rm(password))
    gc(verbose = FALSE)
    cli::cli_abort(c("Failed to parse stored key file.",
                     "i" = "Possibly corrupted or incompatible format.",
                     "Original error message: ",
                     e$message))
  })

  if (!all(c("version", "salt", "encrypted_key") %in% names(key_data))) {
    suppressWarnings(rm(password, key_data))
    gc(verbose = FALSE)
    cli::cli_abort(c("Invalid key file format.", "x" = "Aborting..."))
  }

  # Prompt for password if not provided
  if (is.null(password)) {
    if (interactive()) {
      pass_prompt <- "Enter the password to decrypt your API key: "
      password <- openssl::askpass(pass_prompt)
      if (is.null(password)) {
        suppressWarnings(rm(key_data))
        cli::cli_abort("Cancelled by user.",
                       "x" = "Aborting.")
      }
    } else {
      rm(key_data)
      cli::cli_abort("Password must be provided in non-interactive sessions.",
                     "x" = "Aborting.")
    }
  }

  # Derive encryption key and decrypt data
  tryCatch({
    db_key <- openssl::bcrypt_pbkdf(password, key_data$salt, 64L, 32L)
  }, error = function(e) {
    suppressWarnings(rm(password, key_data))
    gc(verbose = FALSE)
    cli::cli_abort(c("Failure deriving decryption key from the password.",
                     "Original error message: ",
                     e$message))
  })
  suppressWarnings(rm(password))

  tryCatch({
    result <- unserialize(openssl::aes_gcm_decrypt(key_data$encrypted_key, db_key))
  }, error = function(e) {
    suppressWarnings(rm(key_data, db_key))
    gc(verbose = FALSE)
    cli::cli_abort(c("Failed to decrypt API key.",
                     "i" = "Possibly wrong password or corrupted file.",
                     "Original error message: ",
                     e$message))
  })

  if (!exists(".rcoinbaseapi_key_mem_store", mode = "environment")) {
    suppressWarnings(rm(key_data, db_key, result))
    gc(verbose = FALSE)
    cli::cli_abort(c("Unable to store key information in memory.",
                     "x" = "Aborting."))
  }
  assign("name", result$name, envir = .rcoinbaseapi_key_mem_store)
  assign("key", openssl::read_key(result$key), envir = .rcoinbaseapi_key_mem_store)
  if (timeout > 0) {
    later::later(apikey_mem_clean,
                 timeout)
  }


  # Cleanup
  suppressWarnings(rm(db_key, key_bin, key_data, result))
  gc(verbose = FALSE)

  return(invisible(TRUE))
}


#' API Key - Clean Memory
#'
#' Clean API keys stored in memory.
#'
#' @returns TRUE if no errors happen.
#' @export
#'
#' @examples
#' apikey_mem_clean()
apikey_mem_clean <- function() {

  if (exists(".rcoinbaseapi_key_mem_store", mode = "environment")) {
    try(rm(ls(envir = .rcoinbaseapi_key_mem_store)),
        silent = TRUE)
    gc(verbose = FALSE)
    cli::cli_inform(c("i" = "API key erased from memory."))
  }
  invisible(TRUE)
}
