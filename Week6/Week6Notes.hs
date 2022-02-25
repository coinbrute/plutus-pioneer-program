-- this week will focus on putting together the pieces learned so far. 

-- we will learn about oracles or things that reach off chain and get real world info on chain for use in contracts

-- this week will build a dApp and a set of executables in a mock chain 

-- there are various ways to provide oracle implementations 
 -- we will use one data provider 
 -- we will use this one provider for one feed to convert usd to ada

-- the risks with oracles come with trusting of the data sources and risks of validation from the outside sources
 -- you can mitigate this by requiring provider to provide collatoral 
 -- you can aggregate sources and only provide data that all sources agree on think like a mini governance 

-- in order for anything to happen there must be a utxo
 -- so we represent the current oracle as a utxo
 -- the datums value field is the current value from the provider of the oracle

-- we can use the concepts from the nft lecture and its uniqueness to ensure that the data from the oracle is unique and correct by attaching an nft value to the script output at the utxo address

-- so how would we use this?
 -- the utxo oracle with the value and the nft attached is the one we need as it is the unique one

-- the oracle must be able to work with contract not written at the time when the oracle was created
-- consider a swap contract where someone can deposit ada and someone else can withdraw it in some usd native token
-- fees must be paid the oracle 
-- current oracle utxo must be consumed
-- nft must be present on oracle utxo
-- redeemer tx must have more than enough to cover tx 

-- the oracle owner can update the oracle values through an update function and is able to collect fees at this transaction step as well
-- tx's can use the oracle via a use function 

