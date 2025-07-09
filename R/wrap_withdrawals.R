
#' Coinbase API - Withdraw - List Withdrawals
#'
#' This function queries the `v2/accounts/{account_uuid}/withdrawals` endpoint to retrieve a list of
#' withdraw transactions associated with the specified account. Requires authentication.
#'
#' @param account_uuid A string containing the account UUID.
#'
#' @returns A data.frame of withdraw transactions associated with the account.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # List all withdraw for an account
#' withdraw <- withdraw_list("c48f20b0-1234-4bd8-90c2-abcde1234567")
#' }
withdraw_list <- function(account_uuid) {

  check_uuid(account_uuid, "account_uuid")

  resp <- apirequest("GET",
                     "v2/accounts",
                     path_params = c(account_uuid, "withdrawals"),
                     need_auth = TRUE,
                     use_sandbox = FALSE)

  resp_data <- httr2::resp_body_json(resp, simplifyVector = TRUE, flatten = TRUE)

  invisible(resp_data$data)
}


#' Coinbase API - Withdraw - Create New Withdraw
#'
#' This function queries the `v2/accounts/{account_uuid}/withdrawals` endpoint to initiate a new
#' withdraw into the specified account using a linked payment method. Requires authentication.
#'
#' @param account_uuid A string containing the account UUID.
#' @param amount A numeric value specifying the amount to withdraw.
#' @param currency A string with the currency code (e.g. `"USD"`, `"EUR"`).
#' @param pay_method_uuid A string containing the payment method UUID.
#' @param auto_confirm A logical value; if TRUE (default), the withdraw is committed immediately.
#'
#' @returns A list with details of the newly created withdraw request.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a new withdraw of 100 USD using a payment method
#' result <- withdraw_new("c48f20b0-1234-4bd8-90c2-abcde1234567",
#'                        amount = 100,
#'                        currency = "USD",
#'                        pay_method_uuid = "d77f67ef-3456-4fd9-a020-7890fe45d0e1")
#' }
withdraw_new <- function(account_uuid, amount, currency, pay_method_uuid, auto_confirm = TRUE) {

  check_uuid(account_uuid, "account_uuid")
  check_positive_number(amount, "amount")
  check_currency_iso(currency, "currency")
  check_uuid(pay_method_uuid, "pay_method_uuid")
  check_boolean(auto_confirm, "auto_confirm")

  wdraw_info <- list(amount = as.character(amount),
                     currency = currency,
                     payment_method = pay_method_uuid,
                     commit = auto_confirm)

  resp <- apirequest("POST",
                     "v2/accounts",
                     path_params = c(account_uuid, "withdrawals"),
                     body_params = wdraw_info,
                     need_auth = TRUE,
                     use_sandbox = FALSE)

  resp_data <- httr2::resp_body_json(resp, simplifyVector = TRUE, flatten = TRUE)[[1]]

  invisible(resp_data)
}


#' Coinbase API - Withdraw - Confirm Pending Withdraw
#'
#' This function queries the `v2/accounts/{account_uuid}/withdrawals/{withdraw_uuid}/commit`
#' endpoint to manually confirm (commit) a previously created withdraw request. Requires
#' authentication.
#'
#' @param account_uuid A string containing the account UUID.
#' @param withdraw_uuid A string containing the withdraw UUID.
#'
#' @returns A list with the updated status of the committed withdraw.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Confirm a pending withdraw
#' result <- withdraw_confirm("c48f20b0-1234-4bd8-90c2-abcde1234567",
#'                            "01234567-89ab-4cde-9f01-23456789abcd")
#' }
withdraw_confirm <- function(account_uuid, withdraw_uuid) {

  check_uuid(account_uuid, "account_uuid")
  check_uuid(withdraw_uuid, "withdraw_uuid")

  resp <- apirequest("POST",
                     "v2/accounts",
                     path_params = c(account_uuid, "withdrawals", withdraw_uuid, "commit"),
                     need_auth = TRUE,
                     use_sandbox = FALSE)

  resp_data <- httr2::resp_body_json(resp, simplifyVector = TRUE, flatten = TRUE)[[1]]

  invisible(resp_data)
}
