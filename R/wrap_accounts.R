
#' Coinbase API - Accounts - List Accounts
#'
#'
#' This function queries the `/api/v3/brokerage/accounts` Coinbase API endpoint.
#'
#'
#' Requires authentication unless `use_sandbox = TRUE`.
#'
#' Retrieves a list of brokerage accounts associated with the authenticated Coinbase user.
#' Automatically handles pagination and returns a combined data frame.
#'
#' Input validation ensures that malformed or malicious values are rejected early. Use of
#' this function in production environments should follow best practices for API key management.
#'
#' @param limit       Optional. Integer. Maximum number of results per page (1–250). If not
#'                    provided, API default applies. Values above 250 will be capped.
#' @param cursor      Optional. Character. Cursor string for pagination (normally obtained from a
#'                    previous call).
#' @param use_sandbox Boolean. If TRUE, uses Coinbase sandbox environment. Default: FALSE.
#'
#' @return A data frame containing account details. The result includes all pages if pagination is
#' required.
#'
#' @seealso [apirequest()]
#'
#' @export
#' @examples
#' \dontrun{
#' # Retrieve list of all brokerage accounts
#' accounts <- account_list()
#'
#' # Use sandbox mode (no authentication required)
#' accounts_sandbox <- account_list(use_sandbox = TRUE)
#' }
#'
account_list <- function(limit = NULL, cursor = NULL, use_sandbox = FALSE) {

  q_par <- list()

  if (!is.null(limit)) {
    check_positive_integer(limit, "limit")
    if (limit > 250L) {
      limit <- 250L
      cli::cli_alert_info("Parameter 'limit = {limit}' exceeds maximum of 250. Reduced to 250.")
    }
    q_par <- c(q_par, limit = as.integer(limit))
  }

  if (!is.null(cursor)) {
    check_string_nonempty(cursor, "cursor")
    q_par <- c(q_par, cursor = cursor)
  }

  check_boolean(use_sandbox, "use_sandbox")

  ret <- data.frame()
  send_request <- TRUE
  while (send_request) {

    resp <- apirequest("GET",
                       "api/v3/brokerage/accounts",
                       query_params = q_par,
                       need_auth = !use_sandbox,
                       use_sandbox = use_sandbox)

    resp_data <- httr2::resp_body_json(resp, simplifyVector = TRUE, flatten = TRUE)

    ret <- rbind(ret, resp_data$accounts)
    send_request <- resp_data$has_next
    q_par$cursor <- resp_data$cursor
  }

  invisible(ret)
}


#' Coinbase API - Accounts - Account Details
#'
#'
#' This function queries the `/api/v3/brokerage/accounts/<acc_uuid>` Coinbase API endpoint.
#'
#'
#' Requires authentication unless `use_sandbox = TRUE`.
#'
#' Retrieves details of the selected brokerage account associated with the authenticated Coinbase
#' user.
#'
#' Input validation ensures that malformed or malicious values are rejected early. Use of
#' this function in production environments should follow best practices for API key management.
#'
#' @param account_uuid Character. Desired account UUID.
#' @param use_sandbox  Boolean. If TRUE, uses Coinbase sandbox environment. Default: FALSE.
#'
#' @return A list containing account details.
#'
#' @seealso [apirequest()]
#'
#' @export
#' @examples
#' \dontrun{
#' # Retrieve list of the brokerage account. Use sandbox mode (no authentication required)
#' account <- account_list("66f975a6-bb2e-44be-82e9-cd8669e404b0", use_sandbox = TRUE)
#' }
account_get <- function(account_uuid = NULL, use_sandbox = FALSE) {

  check_uuid(account_uuid, "account_uuid")

  check_boolean(use_sandbox, "use_sandbox")

  resp <- apirequest("GET",
                     "api/v3/brokerage/accounts",
                     path_params = account_uuid,
                     need_auth = !use_sandbox,
                     use_sandbox = use_sandbox)

  resp_data <- httr2::resp_body_json(resp, simplifyVector = TRUE, flatten = TRUE)[[1]]

  invisible(resp_data)
}
