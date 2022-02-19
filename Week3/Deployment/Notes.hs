-- so I will not be actually spinning up a live node as that is f'ing huge. 
-- here is the process though for deploying to the testnet
 -- the mainnet is not much different

-- spin up the node using the configuration files 
 -- in this case I am using the tetnet ones but using the iohk cardano-node github repo you can get the mainnet config files as well
 -- https://github.com/input-output-hk/cardano-node/releases/tag/1.33.0
 -- use the bash script ./start-node-testnet.sh to start node
-- once node has sync'd do the following

-- 1. using cardano-cli in a nix-shell do all the following

-- 2. create some keys 
 {-
  cardano-cli address key-gen --verification-key-file 01.vkey --signing-key-file 01.skey
  
  cardano-cli address key-gen --verification-key-file 02.vkey --signing-key-file 02.skey
 -}
 -- the above command creates four keys
  -- two verification keys and two signing keys one each for 01 and 02.
 -- they will show up in the folder the command is run in
 -- you can cat out the file and it is json-esque

-- 3. make addresses for these keys  
 -- you will need to specify the payment verification key file from the last step
 -- must also specify whether you are on mainnet or testnet
  -- if on the testnet then you must specify a testnet Magic ID
   -- this is contained in the Shelley Genesis Json File in the config stuff under "networkMagic":
 {- 
  cardano-cli address build --payment-verification-key-file 01.vkey --testnet-magic <networkMagic> --out-file 01.addr

  cardano-cli address build --payment-verification-key-file 02.vkey --testnet-magic <networkMagic> --out-file 02.addr
 -}

-- 4. Get some test ADA from the faucet with these addresses
 -- go to testnets.cardano.org/en/testnets/cardano/tools/faucet/
 -- request the ADA

-- 5. Verify it worked
 -- look up the utxos for the addresses that requested the ADA
 -- there is an environment variable that must be set to match the local node socket 
  -- get this by: 
  {-
   export CARDANO_NODE_SOCKET_PATH=node.socket

   cardano-cli query utxo --address $(cat 01.addr) --testnet-magic <networkMagic>
  -}
 -- this outputs the tx hash, the output index and the amount 

-- 6. use the cardano-cli to build a transaction to send some ada from wallet 01 to wallet 02 since you can only use the faucet once every 24 hours. 
-- it can be found in send.sh also written out here
{-
* build the transaction
 cardano-cli transaction build \
  --alonzo-era \ ** specify era since default is different
  --testnet-magic <networkMagic> \ ** give testnet magic
  --change-address $(cat 01.addr) \ ** give first address
  --tx-in <address> \ ** address of sending address with #0 appended
  --tx-out "$(cat 02.addr) 10000000 locelace" \ ** recipient address with amount being sent in lovelace
  --out-file tx.body ** file to create tx

* sign the transaction
 cardano-cli transaction sign \
  --tx-body-file tx.body \ ** file to get tx info from
  --signing-key-file 01.skey \ ** wallet signing file to use
  --testnet-magic <networkMagic> \ ** give testnet magic
  --out-file tx.signed ** output signed tx file

* submit the signed transaction
 cardano-cli transaction submit \
  --testnet-magic <networkMagic> \ ** give testnet magic
  --tx-file tx.signed ** get signed tx file
-}

-- 7. send the above script with
 -- ./send.sh

-- 8. verify it worked by running the following until you see the change in utxos
{-
 cardano-cli query utxo --address $(cat 01.addr) --testnet-magic <networkMagic>
-}

-- 9. you can then verify the ADA got the other wallet by running 
{-
 cardano-cli query utxo --address $(cat 02.addr) --testnet-magic <networkMagic>
-}

-- in order to work with your plutus scripts within the cardano-cli you need to serialize and write to disk various plutus types hence the Deploy module 

-- we use the Cardano.Api which has functionality to talk to the Cardano node
 -- it has a Data data type which mimics the plutus Data data type 
 -- we first need to convert to use the plutus stuff on cardano node

-- 10. go over to Deploy.hs and follow the notes there

-- 11. take a look at ./give.hs
{-
cardano-cli transaction build \
    --alonzo-era \ ** specify era
    --testnet-magic 1097911063 \ ** give testnet magic
    --change-address $(cat 01.addr) \ ** specify input address
    --tx-in abae0d0e19f75938537dc5e33252567ae3b1df1f35aafedd1402b6b9ccb7685a#0 \ ** specificy sender address with the #0 on the end
    --tx-out "$(cat vesting.addr) 200000000 lovelace" \ ** specify the recipient and amount
    --tx-out-datum-hash-file unit.json \ ** create the datum file using unit json we made in the Deploy module
    --out-file tx.body ** save the tx file

cardano-cli transaction sign \
    --tx-body-file tx.body \  ** load the tx file
    --signing-key-file 01.skey \ ** specify the signer 
    --testnet-magic 1097911063 \ ** give the testnet magic
    --out-file tx.signed ** specify the out file

cardano-cli transaction submit \
    --testnet-magic 1097911063 \ ** give the testnet magic
    --tx-file tx.signed ** specify the file
-}

-- 12. run ./give.hs

-- 13. check the transaction
{-
cardano-cli query utxo --address $(cat vesting.addr) --testnet-magic <networkMagic>
-}

-- 14. take a look at ./grab.hs
{-
cardano-cli transaction build \
    --alonzo-era \ ** get era
    --testnet-magic 1097911063 \ ** give testnet magic
    --change-address $(cat 02.addr) \ ** give input address
    --tx-in 18cbe6cadecd3f89b60e08e68e5e6c7d72d730aaa1ad21431590f7e6643438ef#1 \ ** specify the input address or spending address
    --tx-in-script-file vesting.plutus \ ** specify the script to use 
    --tx-in-datum-file unit.json \ ** specify the datum
    --tx-in-redeemer-file unit.json \ ** specify the redeemer
    --tx-in-collateral 18e93407ea137b6be63039fd3c564d4c5233e7eb7ce4ee845bc7df12c80e4df7#1 \ ** this is for security against DOS attacks but mainly to gaurd against failures and to ensure you verify in wallet instead of directly against the node. It is a small fee to pay in the event verification fails on node. so here you specify a utxo that belongs to you that can cover the costs if validation fails 
    --required-signer-hash c2ff616e11299d9094ce0a7eb5b7284b705147a822f4ffbd471f971a \ ** specify the signer address and explicity say this is required. this is the key hash computed earlier
    --invalid-before 48866954 \ time the transaction is valid from in slots. get the current slot by running ```cardano-cli query tip --testnet-magic <networkMagic>``` field "slot"
    --protocol-params-file protocol.json \ ** get this file by running ```cardano-cli query protocol-parameters --out-file protocol.json```
    --out-file tx.body ** body file 

cardano-cli transaction sign \
    --tx-body-file tx.body \ ** load body file
    --signing-key-file 02.skey \ ** signing key file
    --testnet-magic 1097911063 \ ** testnet magic
    --out-file tx.signed ** out file

cardano-cli transaction submit \
    --testnet-magic 1097911063 \ ** testnet magic
    --tx-file tx.signed ** out file
-}

-- 15. run ./grab.hs

-- 16. check the transaction
{-
cardano-cli query utxo --address $(cat 02.addr) --testnet-magic <networkMagic>
-}

-- Mainnet is the same except you use --mainnet-magic instead and use the mainnet configurations instead of the testnet ones.

