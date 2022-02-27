-- this week lecture focuses on testing and token sales
-- the contract is for somebody to be able to sell tokens for a given price from a contract where they have locked them

-- the utxo starts from the seller with the nft
-- the seller locks the nft at the script address with a tx 
 -- the output is the token sale with a datum of the nft with an integer for the price of the token 
-- sellers can set the price of the token by submitting a tx with the current nft utxo as input
 -- output of nft with new price as datum integer
-- sellers can add tokens with a tx input of tokens 
 -- output would be nft + tokens and datum of the integer price
-- to buy tokens somebody inputs a tx with the amount
 -- the output of the tokens
 -- the token sale and the nft plust the ada amount and the tokens left and the datum integer price
-- seller can withdraw
 -- output is the token sale with nft + tokens and datum of integer price 
 -- the sellers token and ada amounts


-- plutus uses the Tasty testing framework 
-- tests are of type TestTree 
 -- these are a tree so there can be sub tests and sub sub tests branches so on and so forth 
-- they work with emulators and property based testing 

-- there is another framework called QuickCheck used as well for automated testing of property based systems 
 -- this is a robust automated famework that allows for modules to be tested via their properties in the funcitons 
 -- it allows for both side effectless and IO testing 
 -- for IO you define idealized models of the states to interact with and then QuickCheck operates tests on randomized sets of thoses models which it spins up actions to be performed on them
  -- in the test suite it will syncronize between the model and the system each test run 
  -- it performs the action and decides the outcome of the test it will shrink the test set if necessary and reevaluate until there is no more ability to reporoduce the bug
  -- this is how qc tests a plutus contract
   -- we define a model of the contract and define all the properties of how the tests will interact with the contract 
   -- we provide a link between the emulator system and the model
   -- we define how the endpoints can change the model
   -- we provide the actions for the qc 
  -- something to note with quickcheck is it is not great with handling concurrency. that is why we keep saying (wait 1) after each call. 
   -- hence there are limitations but still a powerful tool for testing.