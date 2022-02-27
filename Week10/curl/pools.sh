#1/bin/sh

# grab the first cmd line arg and cat out the cid file it associates to. plop it in the pools endpoint and fire at will
 # the request body is empty
curl  "http://localhost:9080/api/contract/instance/$(cat W$1.cid)/endpoint/pools" \
--header 'Content-Type: application/json' \
--data-raw '[]'
