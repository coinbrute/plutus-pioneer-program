#1/bin/sh

# just takes the wallet cid and calls the funds endpoint and makes the post request with the data 
curl  "http://localhost:9080/api/contract/instance/$(cat W$1.cid)/endpoint/funds" \
--header 'Content-Type: application/json' \
--data-raw '[]'
