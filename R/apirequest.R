
#' API Request - Request
#'
#' @param method     HTTP method to use. Defaults to GET
#' @param endpoint   The endpoint to send the request to
#' @param need_auth  Does this request need authorization?
#'
#' @returns the API response
#' @export
apirequest <- function(method = "GET", endpoint = NULL, need_auth = TRUE) {

  base_url <- "https://api.coinbase.com"

  req <- httr2::request(base_url)
  req <- httr2::req_method(req, method)
  req <- httr2::req_url_path(req, endpoint)
  req <- httr2::req_headers(req, "content-type" = "application/json")
  if (need_auth) {
    req <- apirequest_authorize(req)
  }

  tryCatch({
    resp <- httr2::req_perform(req)
  },
  error = function(e) {
    cli::cli_abort(c("Failure executing the request.",
                      e$message))
  })

  invisible(resp)
}


#' API Request - Request Authorize
#' @param req  An httr2 request
#' @returns The httr2 request received as parameter with an authorization header
apirequest_authorize <- function(req) {

  url <- httr2::url_parse(req$url)
  uri <- paste(req$method, paste0(url$hostname, url$path))

  if (!exists(".rcoinbaseapi_key_mem_store", mode = "environment")) {

    cli::cli_abort(c("Cannot find in-memory key vault.",
                     "x" = "Aborting."))

  } else if (!all(c("name", "key") %in% ls(envir = .rcoinbaseapi_key_mem_store))) {

    cli::cli_abort(c("Signing key information not loaded or already expired.",
                     "i" = "Try to run {.fun rcoinbaseapi::apikey_read} to solve this problem.",
                     "x" = "Aborting."))

  } else {

    payload <- jose::jwt_claim(iss = "cdp",
                               sub = .rcoinbaseapi_key_mem_store$name,
                               nbf = as.integer(Sys.time()),
                               exp = as.integer(Sys.time()) + 120L,
                               uri = uri)

    head <- list(kid = .rcoinbaseapi_key_mem_store$name,
                 nonce = paste0(as.character(openssl::rand_bytes(32)), collapse = ""))
    token <- jose::jwt_encode_sig(payload, .rcoinbaseapi_key_mem_store$key, header = head)

    req <- httr2::req_auth_bearer_token(req, token)
  }

  invisible(req)
}
