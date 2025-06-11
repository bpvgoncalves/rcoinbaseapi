
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

  if (need_auth) {
    req <- apirequest_authorize(req)
  }

  resp <- httr2::req_perform(req)

  invisible(resp)
}


#' API Request - Request Authorize
#'
#' @param req  An httr2 request
#'
#' @returns The httr2 request received as parameter with an authorization header
#' @export
apirequest_authorize <- function(req) {

  url <- httr2::url_parse(req$url)
  uri <- paste(req$method, paste0(url$hostname, url$path))

  payload <- httr2::jwt_claim(iss = "cdp",
                              sub = key_mem_store$name,
                              nbf = as.integer(Sys.time()),
                              exp = as.integer(Sys.time()) + 120L,
                              uri = uri)

  head <- list(kid = key_mem_store$name,
               nonce = paste0(as.character(openssl::rand_bytes(32)), collapse = ""))
  token <- httr2::jwt_encode_sig(payload, key_mem_store$key, header = head)

  req <- httr2::req_auth_bearer_token(req, token)
  invisible(req)
}
