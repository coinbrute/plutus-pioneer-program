#1/bin/sh

# again we get the symbol and the body using file cat or cmd line args then echo 
symbol=$( cat symbol.json )
body="{\"apAmountA\":$2,\"apAmountB\":$4,\"apCoinB\":{\"unAssetClass\":[$symbol,{\"unTokenName\":\"$5\"}]},\"apCoinA\":{\"unAssetClass\":[$symbol,{\"unTokenName\":\"$3\"}]}}"
echo $body

# input the first cmd line arg and cat out in the call to add endpoint and input body to request body
curl  "http://localhost:9080/api/contract/instance/$(cat W$1.cid)/endpoint/add" \
--header 'Content-Type: application/json' \
--data-raw $body
