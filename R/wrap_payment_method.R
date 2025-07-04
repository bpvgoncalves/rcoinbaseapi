
#' Coinbase API - Management - List Payment Methods
#'
#' @returns A data.frame with the payment method details.
#' @export
#'
#' @examples
#' \dontrun{
#' pay_list <- pay_method_list()
#' }
pay_method_list <- function() {


  resp <- apirequest("GET",
                     "api/v3/brokerage/payment_methods",
                     need_auth = TRUE,
                     use_sandbox = FALSE)

  resp_data <- httr2::resp_body_json(resp, simplifyVector = TRUE, flatten = TRUE)[[1]]

  invisible(resp_data)

}


#' Coinbase API - Management - Get Payment Method
#'
#' @param method_uuid Character. The UUID for the desired payment method.
#'
#' @returns A named list with the details for the requested payment method.
#' @export
#'
#' @examples
#' \dontrun{
#' pay <- pay_method_get("00000000-0000-4000-8000-000000000000")
#' }
pay_method_get <- function(method_uuid) {

  if (is.null(method_uuid) || is_ugly(method_uuid) || !is_uuid(method_uuid)) {
    cli::cli_abort("Invalid parameter `method_uuid`: {method_uuid}")
  }


  resp <- apirequest("GET",
                     "api/v3/brokerage/payment_methods",
                     path_params = method_uuid,
                     need_auth = TRUE,
                     use_sandbox = FALSE)

  resp_data <- httr2::resp_body_json(resp, simplifyVector = TRUE, flatten = TRUE)[[1]]

  invisible(resp_data)
}
