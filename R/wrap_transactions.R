
#' Coinbase API - Transactions - List Transactions
#'
#'
#' This function queries the `/v2/accounts/<accid>/transactions` Coinbase API endpoint. Requires
#' authentication.
#'
#' Retrieves a list of transactions associated with the account passed as parameter.
#'
#' Input validation ensures that malformed or malicious values are rejected early. Use of
#' this function in production environments should follow best practices for API key management.
#'
#' @param account_uuid  Character. Desired account UUID.
#'
#' @returns A data frame containing transaction details.
#' @export
#'
#' @examples
#' \dontrun{
#' # Retrieve list of all transactions for the account
#' accounts <- transaction_list("00000000-0000-4000-8000-000000000000")
#' }
transaction_list <- function(account_uuid) {

  if (is.null(account_uuid) || is_ugly(account_uuid) || !is_uuid(account_uuid)) {
    cli::cli_abort("Invalid parameter `account_uuid`: {account_uuid}")
  }

  resp <- apirequest("GET",
                     "v2/accounts",
                     path_params = c(account_uuid, "transactions"),
                     need_auth = TRUE,
                     use_sandbox = FALSE)

  resp_data <- httr2::resp_body_json(resp, simplifyVector = TRUE, flatten = TRUE)[[1]]

  invisible(resp_data)

}


#' Coinbase API - Transactions - Transaction Details
#'
#'
#' This function queries the `/v2/accounts/<accid>/transactions/<transid>` Coinbase API endpoint.
#' Requires authentication.
#'
#' Retrieves details associated with the account and transaction passed as parameters.
#'
#' Input validation ensures that malformed or malicious values are rejected early. Use of
#' this function in production environments should follow best practices for API key management.
#'
#' @param account_uuid      Character. Desired account UUID.
#' @param transaction_uuid  Character. Desired transaction UUID.
#'
#' @returns A list containing transaction details.
#' @export
#'
#' @examples
#' \dontrun{
#' # Retrieve list of transaction details
#' accounts <- transaction_list("00000000-0000-4000-8000-000000000000",
#'                              "11111111-1111-4111-9111-111111111111")
#' }
transaction_get <- function(account_uuid, transaction_uuid) {

  if (is.null(account_uuid) || is_ugly(account_uuid) || !is_uuid(account_uuid)) {
    cli::cli_abort("Invalid parameter `account_uuid`: {account_uuid}")
  }

  if (is.null(transaction_uuid) || is_ugly(transaction_uuid) || !is_uuid(transaction_uuid)) {
    cli::cli_abort("Invalid parameter `transaction_uuid`: {transaction_uuid}")
  }

  resp <- apirequest("GET",
                     "v2/accounts",
                     path_params = c(account_uuid, "transactions", transaction_uuid),
                     need_auth = TRUE,
                     use_sandbox = FALSE)

  resp_data <- httr2::resp_body_json(resp, simplifyVector = TRUE, flatten = TRUE)[[1]]

  invisible(resp_data)
}


#' Coinbase API - Transactions - Transactions Summary
#'
#' Retrieves a summary of user transactions from the
#' `/api/v3/brokerage/transaction_summary` endpoint. Authentication is required.
#'
#' The response includes total trading volume, total fees paid, and maker/taker fee tiers.
#'
#' @param product_type Optional. One of `"UNKNOWN_PRODUCT_TYPE"`, `"SPOT"`, or `"FUTURE"`.
#' @param expiry Optional. One of `"UNKNOWN_CONTRACT_EXPIRY_TYPE"`, `"EXPIRING"`, or `"PERPETUAL"`.
#'   Only applicable when `product_type = "FUTURE"`.
#' @param venue Optional. One of `"UNKNOWN_VENUE_TYPE"`, `"CBE"`, `"FCM"`, or `"INTX"`.
#'
#' @returns A named list containing at least the following:
#' - **total_volume**: Total volume across assets, denoted in USD.
#' - **total_fees**: Total fees across assets, denoted in USD.
#' - **fee_tier**: A list with the description of maker and taker rates across all products.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Retrieve list of transactions for the account
#' tx_summary <- transaction_summary()
#' }
transaction_summary <- function(product_type = NULL, expiry = NULL, venue = NULL) {

  q_par <- list()

  if (!is.null(product_type)) {
    acceptable_type <- c("UNKNOWN_PRODUCT_TYPE", "SPOT", "FUTURE")
    if (!is_ugly(product_type) && toupper(product_type) %in% acceptable_type) {
      q_par <- c(q_par, product_type = toupper(product_type))
    } else {
      cli::cli_abort(c("x" = "Invalid parameter `product_type`: {product_type}",
                       "i" = "It MUST be one of: {acceptable_type}"))
    }
  }

  if (!is.null(expiry)) {
    acceptable_expiry <- c("UNKNOWN_CONTRACT_EXPIRY_TYPE", "EXPIRING", "PERPETUAL")
    if (!is_ugly(expiry) && toupper(expiry) %in% acceptable_expiry) {
      if (!is.null(q_par$product_type) && q_par$product_type=="FUTURE") {
        q_par <- c(q_par, contract_expiry_type = toupper(expiry))
      } else {
        cli::cli_inform(c("i" = "Ignoring parameter `expiry`",
                          "Parameter `expiry` only applicable when `product_type` is 'FUTURE'."))
      }
    } else {
      cli::cli_abort(c("x" = "Invalid parameter `expiry`: {expiry}",
                       "i" = "It MUST be one of: {acceptable_expiry}"))
    }
  }

  if (!is.null(venue)) {
    acceptable_venue <- c("UNKNOWN_VENUE_TYPE", "CBE", "FCM", "INTX")
    if (!is_ugly(venue) && toupper(venue) %in% acceptable_venue) {
      q_par <- c(q_par, product_venue = toupper(venue))
    } else {
      cli::cli_abort(c("x" = "Invalid parameter `venue`: {venue}",
                       "i" = "It MUST be one of: {acceptable_venue}"))
    }
  }

  resp <- apirequest("GET",
                     "api/v3/brokerage/transaction_summary",
                     query_params = q_par,
                     need_auth = TRUE,
                     use_sandbox = FALSE)

  resp_data <- httr2::resp_body_json(resp, simplifyVector = TRUE, flatten = TRUE)

  invisible(resp_data)
}
