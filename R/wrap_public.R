#' Coinbase API - Public - Server Time
#'
#' Queries the public `/v2/time` endpoint to retrieve the current Coinbase server time.
#'
#' @return A list containing:
#'   -**epoch**: Server time as Unix timestamp (seconds since epoch).
#'   -**iso**: Server time in ISO 8601 format (UTC).
#'
#' @examples
#' server_time()
#'
#' @export
server_time <- function() {

  time_resp <- apirequest("GET", "v2/time", need_auth = FALSE, use_sandbox = FALSE)

  time_data <- httr2::resp_body_json(time_resp, simplifyVector = TRUE, flatten = TRUE)[[1]]

  invisible(time_data)
}
