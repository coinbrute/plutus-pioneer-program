#1/bin/sh

# here we create the symbole variable 
symbol=$( cat symbol.json )
# here we define our body
 # we pass in some cmd line args here for the amounts of a and b as well as the names of a and b
body="{\"cpAmountA\":$2,\"cpAmountB\":$4,\"cpCoinB\":{\"unAssetClass\":[$symbol,{\"unTokenName\":\"$5\"}]},\"cpCoinA\":{\"unAssetClass\":[$symbol,{\"unTokenName\":\"$3\"}]}}"
# echo it out to console 
echo $body

# cat our wallet cid file into our create endpoint call and pass our body into the post request 
curl  "http://localhost:9080/api/contract/instance/$(cat W$1.cid)/endpoint/create" \
--header 'Content-Type: application/json' \
--data-raw $body
