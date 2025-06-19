
#' API Request - Request
#'
#' @param method       String. The HTTP method to use (GET, POST, PUT, DELETE, ...). Default: GET.
#' @param endpoint     String. The endpoint to send the request to.
#' @param path_params  String. Parameters to be added to the request path. Default: NULL.
#' @param query_params Named list. Parameters to be added to the request query. Default: NULL.
#' @param body_params  Named list. Parameters to be sent as the request body. Default: NULL.
#' @param need_auth    Boolean. Does this request need authorization? Default: TRUE
#'
#' @returns the API response
#' @export
apirequest <- function(method = "GET",
                       endpoint = NULL,
                       path_params = NULL,
                       query_params = NULL,
                       body_params = NULL,
                       need_auth = TRUE) {

  base_url <- "https://api.coinbase.com"

  # Create base request
  req <- httr2::request(base_url)

  if (!is.null(method) && !is_ugly(method) && method %in% c("GET", "POST", "PUT", "DELETE")) {
    req <- httr2::req_method(req, method)
  } else {
    cli::cli_abort(c("Invalid parameter `method`: {method}",
                     "i" = "Parameter `method` MUST be: 'GET', 'POST', 'PUT' or 'DELETE'."))
  }

  if (!is.null(endpoint) && !is_ugly(endpoint)) {
    req <- httr2::req_url_path(req, endpoint)
  } else {
    cli::cli_abort("Invalid parameter `endpoint`: {endpoint}")
  }

  # Add parameters, if available
  if (!is.null(path_params)) {
    if (!is_bad(path_params)) {
      req <- httr2::req_url_path_append(req, path_params)
    } else {
      cli::cli_abort("Invalid parameter `path_params`: {path_params}")
    }
  }

  if (!is.null(query_params)) {
    if (!is.list(query_params)) {
      cli::cli_abort(c("Invalid parameter `query_params`.",
                       "i" = "Parameter MUST be a list."))
    }
    if (any(names(query_params) == "")) {
      cli::cli_abort(c("Invalid parameter `query_params`.",
                       "i" = "All items in the list MUST be named."))
    }
    req <- do.call(httr2::req_url_query, c(list(.req = req), query_params))
  }

  if (!is.null(body_params)) {
    if (!is.list(body_params)) {
      cli::cli_abort(c("Invalid parameter `body_params`.",
                       "i" = "Parameter MUST be a list."))
    }
    if (any(names(body_params) == "")) {
      cli::cli_abort(c("Invalid parameter `body_params`.",
                       "i" = "All items in the list MUST be named."))
    }
    req <- httr2::req_body_json(req, jsonlite::toJSON(body_params, auto_unbox = TRUE))
  }

  # Add some headers
  req <- httr2::req_headers(req, "Accept" = "application/json")
  req <- httr2::req_headers(req, "Accept-Encoding" = "gzip, deflate, br, zstd")

  # Add request authorization if needed
  if (!is.null(need_auth) && !is_ugly(need_auth) && is.logical(need_auth)) {
    if (need_auth) {
      req <- apirequest_authorize(req)
    }
  } else {
    cli::cli_abort("Invalid parameter `need_auth`: {need_auth}")
  }

  # Perform request
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
