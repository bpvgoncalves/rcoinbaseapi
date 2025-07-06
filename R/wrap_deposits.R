
#' Coinbase API - Deposit - List Deposits
#'
#' This function queries the `v2/accounts/{account_uuid}/deposits` endpoint to retrieve a list of
#' deposit transactions associated with the specified account. Requires authentication.
#'
#' @param account_uuid A string containing the account UUID.
#'
#' @returns A data.frame of deposit transactions associated with the account.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # List all deposits for an account
#' deposits <- deposit_list("c48f20b0-1234-4bd8-90c2-abcde1234567")
#' }
deposit_list <- function(account_uuid) {

  if (is.null(account_uuid) || is_ugly(account_uuid) || !is_uuid(account_uuid)) {
    cli::cli_abort("Invalid parameter `account_uuid`: {account_uuid}")
  }

  resp <- apirequest("GET",
                     "v2/accounts",
                     path_params = c(account_uuid, "deposits"),
                     need_auth = TRUE,
                     use_sandbox = FALSE)

  resp_data <- httr2::resp_body_json(resp, simplifyVector = TRUE, flatten = TRUE)

  invisible(resp_data$data)
}


#' Coinbase API - Deposit - Create New Deposit
#'
#' This function queries the `v2/accounts/{account_uuid}/deposits` endpoint to initiate a new
#' deposit into the specified account using a linked payment method. Requires authentication.
#'
#' @param account_uuid A string containing the account UUID.
#' @param amount A numeric value specifying the amount to deposit.
#' @param currency A string with the currency code (e.g. `"USD"`, `"EUR"`).
#' @param pay_method_uuid A string containing the payment method UUID.
#' @param auto_confirm A logical value; if TRUE (default), the deposit is committed immediately.
#'
#' @returns A list with details of the newly created deposit request.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a new deposit of 100 USD using a payment method
#' result <- deposit_new("c48f20b0-1234-4bd8-90c2-abcde1234567",
#'                       amount = 100,
#'                       currency = "USD",
#'                       pay_method_uuid = "d77f67ef-3456-4fd9-a020-7890fe45d0e1")
#' }
deposit_new <- function(account_uuid, amount, currency, pay_method_uuid, auto_confirm = TRUE) {

  if (is.null(account_uuid) || is_ugly(account_uuid) || !is_uuid(account_uuid)) {
    cli::cli_abort("Invalid parameter `account_uuid`: {account_uuid}")
  }

  if (is.null(amount) || is_ugly(amount) || !is.numeric(amount)) {
    cli::cli_abort("Invalid parameter `amount`: {amount}")
  }

  if (is.null(currency) || is_ugly(currency) || !is.character(currency)) {
    cli::cli_abort("Invalid parameter `currency`: {currency}")
  }

  if (is.null(pay_method_uuid) || is_ugly(pay_method_uuid) || !is_uuid(pay_method_uuid)) {
    cli::cli_abort("Invalid parameter `pay_method_uuid`: {pay_method_uuid}")
  }

  if (is.null(auto_confirm) || is_ugly(auto_confirm) || !is.logical(auto_confirm)) {
    cli::cli_abort("Invalid parameter `auto_confirm`: {auto_confirm}")
  }

  depo_info <- list(amount = as.character(amount),
                    currency = currency,
                    payment_method = pay_method_uuid,
                    commit = auto_confirm)

  resp <- apirequest("POST",
                     "v2/accounts",
                     path_params = c(account_uuid, "deposits"),
                     body_params = depo_info,
                     need_auth = TRUE,
                     use_sandbox = FALSE)

  resp_data <- httr2::resp_body_json(resp, simplifyVector = TRUE, flatten = TRUE)[[1]]

  invisible(resp_data)
}


#' Coinbase API - Deposit - Confirm Pending Deposit
#'
#' This function queries the `v2/accounts/{account_uuid}/deposits/{deposit_uuid}/commit` endpoint to
#' manually confirm (commit) a previously created deposit request. Requires authentication.
#'
#' @param account_uuid A string containing the account UUID.
#' @param deposit_uuid A string containing the deposit UUID.
#'
#' @returns A list with the updated status of the committed deposit.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Confirm a pending deposit
#' result <- deposit_confirm("c48f20b0-1234-4bd8-90c2-abcde1234567",
#'                           "fd0f49b4-9876-4f77-bcfe-7e34ad809ad7")
#' }
deposit_confirm <- function(account_uuid, deposit_uuid) {

  if (is.null(account_uuid) || is_ugly(account_uuid) || !is_uuid(account_uuid)) {
    cli::cli_abort("Invalid parameter `account_uuid`: {account_uuid}")
  }

  if (is.null(deposit_uuid) || is_ugly(deposit_uuid) || !is_uuid(deposit_uuid)) {
    cli::cli_abort("Invalid parameter `deposit_uuid`: {deposit_uuid}")
  }

  resp <- apirequest("POST",
                     "v2/accounts",
                     path_params = c(account_uuid, "deposits", deposit_uuid, "commit"),
                     need_auth = TRUE,
                     use_sandbox = FALSE)

  resp_data <- httr2::resp_body_json(resp, simplifyVector = TRUE, flatten = TRUE)[[1]]

  invisible(resp_data)
}
