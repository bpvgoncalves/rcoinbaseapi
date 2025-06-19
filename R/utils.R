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
