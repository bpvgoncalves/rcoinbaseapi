#' Coinbase API - Public - Server Time
#'
#' Queries the public `/api/v3/brokerage/time` endpoint to get the current Coinbase server time.
#'
#' @return A list containing:
#'   -**iso**: Server time in ISO 8601 format (UTC).
#'   -**epochSeconds**: Server time as Unix timestamp (seconds since epoch).
#'   -**epochMillis**: Server time as Unix timestamp (milliseconds since epoch).
#'
#'
#' @examples
#' server_time()
#'
#' @export
server_time <- function() {

  time_resp <- apirequest("GET", "api/v3/brokerage/time", need_auth = FALSE, use_sandbox = FALSE)

  time_data <- httr2::resp_body_json(time_resp, simplifyVector = TRUE, flatten = TRUE)
  time_data$epochSeconds <- as.numeric(time_data$epochSeconds)
  time_data$epochMillis <- as.numeric(time_data$epochMillis)

  invisible(time_data)
}
