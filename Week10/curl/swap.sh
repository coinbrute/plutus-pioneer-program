#1/bin/sh

# again we get the symbol and the body using file cat or cmd line args then echo 
symbol=$( cat symbol.json )
body="{\"spAmountA\":$2,\"spAmountB\":0,\"spCoinB\":{\"unAssetClass\":[$symbol,{\"unTokenName\":\"$4\"}]},\"spCoinA\":{\"unAssetClass\":[$symbol,{\"unTokenName\":\"$3\"}]}}"
echo $body

# input the first cmd line arg and cat out in the call to swap endpoint and input body to request body
curl  "http://localhost:9080/api/contract/instance/$(cat W$1.cid)/endpoint/swap" \
--header 'Content-Type: application/json' \
--data-raw $body
