% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrap_addresses.R
\name{address_list}
\alias{address_list}
\title{Coinbase API - Onchain Addresses - List Account Addresses}
\usage{
address_list(account_uuid)
}
\arguments{
\item{account_uuid}{A string containing the account UUID.}
}
\value{
A data.frame of addresses associated with the account.
}
\description{
This function queries the \code{v2/accounts/{account_uuid}/addresses} endpoint to retrieve a list of
addresses associated with the specified account. Requires authentication.
}
\examples{
\dontrun{
# List all addresses for an account
addresses <- address_list("c48f20b0-1234-4bd8-90c2-abcde1234567")
}
}
