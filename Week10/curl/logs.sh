#1/bin/sh

# cat out the cid file associated to the first cmd line arg and call the status endpoint 
# use jq to dive into the json and get the logs from the cicCurrentState field
curl  "http://localhost:9080/api/contract/instance/$(cat W$1.cid)/status" | jq ".cicCurrentState.logs"
