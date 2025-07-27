#' @keywords internal
is_bad <- function(x) {
  any(is.na(x)) || any(is.nan(x)) || any(is.infinite(x))
}


#' @keywords internal
is_ugly <- function(x) {
  length(x) != 1 || is_bad(x)
}


#' @keywords internal
check_boolean <- function(value, var_name) {

  ck <- checkmate::check_flag(value)

  if (!is.logical(ck)) ck_fail(value, var_name, ck)
  invisible(TRUE)
}


#' @keywords internal
check_currency_iso <- function(value, var_name) {

  ck <- checkmate::check_string(value, min.chars = 3L, max.chars = 3L)

  if (ck == TRUE && !(toupper(value) %in% dataset_ccy$id)) {
    ck <- "Invalid currency code. Check `currencies()` for more details about acceptable values."
  }

  if (!is.logical(ck)) ck_fail(value, var_name, ck)
  invisible(TRUE)
}


#' @keywords internal
check_enum <- function(value, var_name, choices) {

  check_string_nonempty(value, var_name)

  if (is_ugly(value) || !(toupper(value) %in% choices)) {
    cli::cli_abort(c("x" = "Invalid parameter `{var_name}`: {value}",
                     "i" = "Must be one of: {choices}"))
  }
  invisible(TRUE)
}


#' @keywords internal
check_positive_integer <- function(value, var_name) {

  ck <- checkmate::check_int(value, lower = 0L, upper = 999999999L)

  if (!is.logical(ck)) ck_fail(value, var_name, ck)
  invisible(TRUE)
}


#' @keywords internal
check_positive_number <- function(value, var_name) {

  ck <- checkmate::check_number(value, lower = 0L, upper = 999999999.99)

  if (!is.logical(ck)) ck_fail(value, var_name, ck)
  invisible(TRUE)
}


#' @keywords internal
check_string_nonempty <- function(value, var_name) {

  ck <- checkmate::check_string(value, min.chars = 1L)

  if (!is.logical(ck)) ck_fail(value, var_name, ck)
  invisible(TRUE)
}


#' @keywords internal
check_string_or_empty <- function(value, var_name) {

  ck <- checkmate::check_string(value, min.chars = 0L)

  if (!is.logical(ck)) ck_fail(value, var_name, ck)
  invisible(TRUE)
}


#' @keywords internal
check_uuid <- function(value, var_name) {

  pattern <- "^[0-9A-F]{8}-[0-9A-F]{4}-[45][0-9A-F]{3}-[89AB][0-9A-F]{3}-[0-9A-F]{12}$"
  ck <- checkmate::check_string(value,
                                pattern = pattern,
                                ignore.case = TRUE)

  if (!is.logical(ck)) ck_fail(value, var_name, ck)
  invisible(TRUE)
}


#' @keywords internal
ck_fail <- function(value, var_name, message) {

  cli::cli_abort(c("x" = "Invalid parameter `{var_name}`: {value}",
                   "i" = message))
}
