structure(
  list(
    method = "GET",
    url = "https://api.coinbase.com/v2/time",
    status_code = 200L,
    headers = structure(
      list(
        date = "Tue, 17 Jun 2025 20:34:14 GMT",
        `content-type` = "application/json",
        `grpc-metadata-content-type` = "application/grpc",
        `trace-id` = "REDACTED",
        `cf-cache-status` = "DYNAMIC",
        `report-to` = "{\"endpoints\":[{\"url\":\"https:\\/\\/a.nel.cloudflare.com\\/report\\/v4?s=m700Mqt%2FsMvd8XXZxGjvYXJD619Jv2OmzVKvutJ9x1CUW2GGY1rwpizi8%2BRbrnAkan9U%2FW572NQMhiIcmZUTmOhkT5ytN%2BYz3HiFSCvd90J%2F8HpqrSmiFYSDTtAoO6w6U1E%3D\"}],\"group\":\"cf-nel\",\"max_age\":604800}",
        nel = "{\"success_fraction\":0.01,\"report_to\":\"cf-nel\",\"max_age\":604800}",
        `strict-transport-security` = "max-age=31536000; includeSubDomains; preload",
        `x-content-type-options` = "nosniff",
        `set-cookie` = "REDACTED",
        server = "cloudflare",
        `cf-ray` = "9515539a7ab43153-MAD",
        `content-encoding` = "gzip"
      ),
      redact = character(0),
      class = "httr2_headers"
    ),
    body = charToRaw("{\"data\":{\"iso\":\"2025-06-17T20:34:14Z\",\"epoch\":1750192454}}"),
    cache = new.env(parent = emptyenv())
  ),
  class = "httr2_response"
)
