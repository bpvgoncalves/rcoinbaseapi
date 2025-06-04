apikey_store <- function(key_name, key, password = NULL) {

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
