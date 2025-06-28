
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
    if (!is_ugly(limit) && is.numeric(limit) && limit == as.integer(limit)) {
      if (limit > 250L) {
        limit <- 250L
        cli::cli_alert_info("Parameter 'limit = {limit}' exceeds maximum of 250. Reduced to 250.")
      }
      q_par <- c(q_par, limit = as.integer(limit))
    } else {
      cli::cli_abort("Invalid parameter `limit`: {limit}")
    }
  }


  if (!is.null(cursor)) {
    if (!is_ugly(cursor) && is.character(cursor)) {
      q_par <- c(q_par, cursor = cursor)
    } else {
      cli::cli_abort("Invalid parameter `cursor`: {cursor}")
    }
  }

  if (is.null(use_sandbox) || !is.logical(use_sandbox)) {
    cli::cli_abort("Invalid parameter `use_sandbox`: {use_sandbox}")
  }

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
