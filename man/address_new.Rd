% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrap_addresses.R
\name{address_new}
\alias{address_new}
\title{Coinbase API - Onchain Addresses - New Address}
\usage{
address_new(account_uuid, label = "")
}
\arguments{
\item{account_uuid}{A string containing the account UUID.}

\item{label}{A name/labet for the address to be created}
}
\value{
A list with the address details
}
\description{
This function queries the \code{v2/accounts/{account_uuid}/addresses} endpoint to create a new
addresses associated with the specified account. Requires authentication.
}
\examples{
\dontrun{
# Create new address for an account
address <- address_new("c48f20b0-1234-4bd8-90c2-abcde1234567", "new address name")
}
}
