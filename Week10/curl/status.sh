#1/bin/sh

# bash is straightforward. we pass in the the cat of the cid file we want the status of and jq the json result field we need
curl  "http://localhost:9080/api/contract/instance/$(cat W$1.cid)/status" | jq ".cicCurrentState.observableState"
