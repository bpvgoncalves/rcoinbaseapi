% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrap_payment_method.R
\name{pay_method_get}
\alias{pay_method_get}
\title{Coinbase API - Management - Get Payment Method}
\usage{
pay_method_get(method_uuid)
}
\arguments{
\item{method_uuid}{Character. The UUID for the desired payment method.}
}
\value{
A named list with the details for the requested payment method.
}
\description{
Coinbase API - Management - Get Payment Method
}
\examples{
\dontrun{
pay <- pay_method_get("00000000-0000-4000-8000-000000000000")
}
}
