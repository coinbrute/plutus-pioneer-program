-- Native tokens and minting policies for them
-- how they work and what goes into them. 

-- so this week we will begin by focusing on the value being passed through the datum in the validator scripts
 -- this value could be ada/an nft/some other new native token

-- so we will take a look at how to do that

-- values are a Map of CurrencySymbols to a Mapy of TopkenName's to Integer
 -- a CurrencySymbol is just a wrapper around Bytestring in hexadecimal digits
 -- a TokenName is just a wrapper around ByteString

-- They are constructed with AssetClass which is just a pair of CurrencySymbol and TokenName

-- so a Value is basically saying how many units of each AssetClass are contained in it

-- you don't construct a Value directly with maps you use helper functions to construct them such as lovelaceValueOf like we have been using
-- tokenName of Ada is "" so lovelaceValueOf 123 == Value (Map [(,Map [("",123)])])

-- since Value is a Monoid you can use mappend on them 
 -- lovelaceValueOf 123 <> lovelaceValueOf 10 == Value (Map [(,Map [("",133)])])

-- we can create a singleton of a value by passing a CurrencySymbol a TokenName and Integer to produce a value
 -- these can be appended just like before since it produces a Monoid 

-- we can retrieve from a Value with valueOf by passing a Value a CurrencySymbol a TokenName to get an amount from the key

-- flattenValue will take a Value and flattens it to a list of triples -> (CurrencySymbol, TokenName, Amount)

-- a tx cant create or delete tokens 
 -- so everything that goes in goes out 
  -- exception being fees

-- a script will take more fees if it consumes more memory or takes more steps

-- so how do we create new tokens or mint new tokens?
 -- the CurrencySymbol being in hex is the hash for the minting policy

-- for each native token to mint or burn the CS is looked up which is the hash for the script
 -- the corrosponding script must be contained in the tx and that plus other scripts in tx will be executed
 -- the purpose of these minting scripts are to validate whether the token can mint or burn

-- because Ada has no hash it can never be minted or burned
 -- only the custom native tokens that have hashes can be minted or burned

-- for minting policies the ScriptPurpose will be set to Minting if the txInfoForge Value is set to non-0 

-- where validation scripts have a datum redeemer and context
 -- minting only has the redeemer and the context
  -- since datum sits at somewhere being spent 

-- the transaction provides the redeemer

-- the ScriptPurpose will get Minting with the CurrencySymbol being sent. 