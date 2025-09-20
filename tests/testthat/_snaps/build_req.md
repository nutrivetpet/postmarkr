# build_req() works

    Code
      build_req("/mock", "GET", "test")
    Output
      <httr2_request>
      GET https://api.postmarkapp.com/mock
      Headers:
      * Accept                 : "application/json"
      * X-Postmark-Server-Token: <REDACTED>
      Body: empty

