# build_req_S7() works

    Code
      build_req_S7(client, "/mock", "GET")
    Output
      <httr2_request>
      GET https://api.postmarkapp.com/mock
      Headers:
      * Accept                 : "application/json"
      * X-Postmark-Server-Token: <REDACTED>
      Body: empty
      Options:
      * timeout_ms    : 60000
      * connecttimeout: 0

