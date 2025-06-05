#
safe_stop <- function(msg, ...) {
  suppressWarnings(rm(list = ls(envir = parent.frame()), envir = parent.frame()))
  gc(verbose = FALSE)
  stop(msg, ...)
}

interactive <- NULL

