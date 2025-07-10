
#' Coinbase API - Onchain Addresses - List Account Addresses
#'
#' This function queries the `v2/accounts/{account_uuid}/addresses` endpoint to retrieve a list of
#' addresses associated with the specified account. Requires authentication.
#'
#' @param account_uuid  A string containing the account UUID.
#'
#' @returns A data.frame of addresses associated with the account.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # List all addresses for an account
#' addresses <- address_list("c48f20b0-1234-4bd8-90c2-abcde1234567")
#' }
address_list <- function(account_uuid) {

  check_uuid(account_uuid, "account_uuid")

  resp <- apirequest("GET",
                     "v2/accounts",
                     path_params = c(account_uuid, "addresses"),
                     need_auth = TRUE,
                     use_sandbox = FALSE)

  resp_data <- httr2::resp_body_json(resp, simplifyVector = TRUE, flatten = TRUE)

  invisible(resp_data$data)
}


#' Coinbase API - Onchain Addresses - List Account Address Transactions
#'
#' This function queries the `v2/accounts/{account_uuid}/addresses/{address_uuid}/transactions`
#' endpoint to retrieve a list of addresses associated with the specified account.
#' Requires authentication.
#'
#' @param account_uuid  A string containing the account UUID.
#' @param address_uuid  A string containing the address UUID.
#'
#' @returns A data.frame of transactions associated with the addresses
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # List all addresses for an account
#' tx <- address_transactions_list("c48f20b0-1234-4bd8-90c2-abcde1234567",
#'                                 "12345678-90af-4000-a111-abcdef123456")
#' }
address_transactions_list <- function(account_uuid, address_uuid) {

  check_uuid(account_uuid, "account_uuid")
  check_uuid(address_uuid, "address_uuid")

  resp <- apirequest("GET",
                     "v2/accounts",
                     path_params = c(account_uuid, "addresses", address_uuid, "transactions"),
                     need_auth = TRUE,
                     use_sandbox = FALSE)

  resp_data <- httr2::resp_body_json(resp, simplifyVector = TRUE, flatten = TRUE)

  invisible(resp_data$data)
}


#' Coinbase API - Onchain Addresses - New Address
#'
#' This function queries the `v2/accounts/{account_uuid}/addresses` endpoint to create a new
#' addresses associated with the specified account. Requires authentication.
#'
#' @param account_uuid  A string containing the account UUID.
#' @param label  A name/labet for the address to be created
#'
#' @returns A list with the address details
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create new address for an account
#' address <- address_new("c48f20b0-1234-4bd8-90c2-abcde1234567", "new address name")
#' }
address_new <- function(account_uuid, label = "") {

  check_uuid(account_uuid, "account_uuid")

  b_par <- list()
  if(!is.null(label)) {
    check_string_or_empty(label, "label")
  }
  b_par$name <- label

  resp <- apirequest("POST",
                     "v2/accounts",
                     path_params = c(account_uuid, "addresses"),
                     body_params = b_par,
                     need_auth = TRUE,
                     use_sandbox = FALSE)

  resp_data <- httr2::resp_body_json(resp, simplifyVector = TRUE, flatten = TRUE)

  invisible(resp_data$data)
}
