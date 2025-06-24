
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rcoinbaseapi

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)  
[![R-CMD-check](https://github.com/bpvgoncalves/rcoinbaseapi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bpvgoncalves/rcoinbaseapi/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/bpvgoncalves/rcoinbaseapi/graph/badge.svg)](https://app.codecov.io/gh/bpvgoncalves/rcoinbaseapi)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Overview

**`rcoinbaseapi`** is an experimental R package that aims at providing a
secure and user-friendly interface to the [Coinbase
API](https://docs.cdp.coinbase.com/). It enables programmatic access to
account data, market information, and authenticated trading endpoints
from within R.

> ⚠️ This package is an independent, unofficial client and is **not
> affiliated with, endorsed by, or maintained by Coinbase.** Coinbase is
> a registered trademark and brand of Coinbase, Inc., Oakland, CA.

> ⚠️ This package allows sending *authenticated trading operations* to
> the Coinbase exchange. Please **take extra care to avoid accidental
> orders or exposure of sensitive credentials**. Ensure you fully
> understand the API and its implications before executing trades.

> ⚠️ Despite best-effort protections, security depends on your local
> machine, R session, and password hygiene. **Never share or hard-code
> passwords or private keys.**

## Features

- **Secure Credential Management**: AES encryption for API key storage,
  password-protected access, and in-memory key handling.
- **JWT-based Authentication**: Easily and transparently sign
  authenticated requests. -️ **Simple API Wrappers**: Compose and send
  requests to Coinbase endpoints using familiar R syntax.
- **Modular Design**: Designed with testability, clarity, and
  extensibility in mind using a functional programming approach.

## Security Model

- Credentials are encrypted with AES-256-GCM and derived using
  `bcrypt_pbkdf()` with a user-supplied password.
- Keys are never stored in plain text on disk.
- The private key is only held in memory for the current session, and
  can be cleared manually or programmatically.
- Input validation and structured error handling help minimize
  unintended actions.

## Installation

You can install the development version of **rcoinbaseapi** directly
from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("bpvgoncalves/rcoinbaseapi")
```

## Quick Example

``` r
library(rcoinbaseapi)

# Example: Call a public endpoint (no authentication required)
resp <- apirequest(
  method    = "GET",
  endpoint  = "v2/time",
  need_auth = FALSE
)

# View parsed JSON content
httr2::resp_body_json(resp)
```

    ## $data
    ## $data$iso
    ## [1] "2025-06-24T20:14:37Z"
    ## 
    ## $data$epoch
    ## [1] 1750796077

## Legal Disclaimer

This software is provided “AS-IS”, without warranty of any kind. Use of
this package may involve interaction with financial accounts and
execution of trades. **You are solely responsible for the consequences
of its use.**

## License

This package is licensed under the MIT License. See [LICENSE](LICENSE)
for full details.
