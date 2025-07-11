% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrap_accounts.R
\name{account_list}
\alias{account_list}
\title{Coinbase API - Accounts - List Accounts}
\usage{
account_list(limit = NULL, cursor = NULL, use_sandbox = FALSE)
}
\arguments{
\item{limit}{Optional. Integer. Maximum number of results per page (1–250). If not
provided, API default applies. Values above 250 will be capped.}

\item{cursor}{Optional. Character. Cursor string for pagination (normally obtained from a
previous call).}

\item{use_sandbox}{Boolean. If TRUE, uses Coinbase sandbox environment. Default: FALSE.}
}
\value{
A data frame containing account details. The result includes all pages if pagination is
required.
}
\description{
This function queries the \verb{/api/v3/brokerage/accounts} Coinbase API endpoint.
}
\details{
Requires authentication unless \code{use_sandbox = TRUE}.

Retrieves a list of brokerage accounts associated with the authenticated Coinbase user.
Automatically handles pagination and returns a combined data frame.

Input validation ensures that malformed or malicious values are rejected early. Use of
this function in production environments should follow best practices for API key management.
}
\examples{
\dontrun{
# Retrieve list of all brokerage accounts
accounts <- account_list()

# Use sandbox mode (no authentication required)
accounts_sandbox <- account_list(use_sandbox = TRUE)
}

}
\seealso{
\code{\link[=apirequest]{apirequest()}}
}
