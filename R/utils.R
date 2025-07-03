#'
interactive <- NULL
all <- NULL

#'
.rcoinbaseapi_key_mem_store <- new.env()


is_bad <- function(x) {
  any(is.na(x)) || any(is.nan(x)) || any(is.infinite(x))
}


is_ugly <- function(x) {
  length(x) != 1 || is_bad(x)
}

is_uuid <- function(string) {
  string <- toupper(as.character(string))
  grepl("[0-9A-F]{8}-[0-9A-F]{4}-[45][0-9A-F]{3}-[89AB][0-9A-F]{3}-[0-9A-F]{12}", string)
}


#' Validate Choice Argument
#'
#' Validates a single-character input against a list of accepted choices.
#'
#' @param arg The argument to validate (character scalar).
#' @param name Name of the argument (used in error messages).
#' @param choices Character vector of valid options (will be matched case-insensitively).
#'
#' @return The validated and uppercased version of the argument.
#' @noRd
validate_choice <- function(arg, name, choices) {
  if (is_ugly(arg) || !(toupper(arg) %in% choices)) {
    cli::cli_abort(c("x" = "Invalid parameter `{name}`: {arg}",
                     "i" = "It MUST be one of: {choices}"))
  }
  toupper(arg)
}


#' Coinbase API - Utils - Check Time Drift
#'
#' This function compares your local system clock with Coinbase's public time server,
#' and warns if a material difference is detected. A significant time drift may cause
#' authentication errors when interacting with the Coinbase API.
#'
#' @param threshold Integer number of seconds allowed before warning is issued. Defaults to 30.
#'
#' @return An invisible object of class `rcoinbaseapi_clock_drift`, a list containing:
#' - **local:** Local system time (as Unix epoch).
#' - **server:** Coinbase server time (as Unix epoch).
#' - **time_delta:** Difference between local and server time (in seconds).
#'
#' @examples
#' drift <- check_clock_drift()
#' print(drift)
#'
#' @export
check_clock_drift <- function(threshold = 30) {

  coinbase_time <- server_time()$epochMillis / 1000
  local_time <- as.numeric(Sys.time())
  time_diff <- local_time - coinbase_time

  out <- structure(list(local = local_time,
                        server = coinbase_time,
                        time_delta = time_diff),
                   class = "rcoinbaseapi_clock_drift")

  if (abs(time_diff) >= threshold) {
    print(out)
    cli::cli_alert_warning(c("System clock differs from Coinbase server by {time_diff} seconds. ",
                             "Excessive drift may cause authentication errors. ",
                             "Please sync your system time."))
  }

  invisible(out)
}

#' @export
print.rcoinbaseapi_clock_drift <- function(x, ...) {

  local_dt <- as.POSIXct(x$local, origin = "1970-01-01", tz = "UTC")
  server_dt <- as.POSIXct(x$server, origin = "1970-01-01", tz = "UTC")

  # Needed to keep lintr 'happy'
  force(local_dt)
  force(server_dt)

  cli::cli_h3("Clock Drift")
  cli::cli_inform(c(
    "{.strong System time:} {format(local_dt, '%Y-%m-%d %H:%M:%OS3 %Z')} (epoch: {x$local})",
    "{.strong Server time:} {format(server_dt, '%Y-%m-%d %H:%M:%OS3 %Z')} (epoch: {x$server})",
    "{.strong Time delta:}  {x$time_delta} seconds"
  ))

  invisible(x)
}
