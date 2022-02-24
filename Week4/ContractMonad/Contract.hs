{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Week4.Contract where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import Data.Void                  (Void)
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

-- Contract w s e a
    -- the contract monad defines code that will run in the wallet i.e. the off chain code
    -- there are four type params 
        -- (w) is similar to the Writer. 
            -- It allows the Contract to write messages of type (w). 
            -- The purpose is to communicate between contracts. 
            -- The (w) type is visible to the outside world and between contracts
        -- (s) for the endpoints available in the contract
        -- (e) is for error message. 
            -- Similar to Maybe and Either with String messages 
            -- but this is more general and you can catch errors and handle them inside the Contract moand
        -- (a) is the result type of the computation
-- EmulatorTrace a
    -- the emulator trace monad is a monad to manually do what is visually simulated in the playground through trace logging

-- this example will do the following for its construction in the type declaration
    -- declare a new Contract
    -- no communication of state will happen so we use Unit () for (w)
    -- we have no endpoints so we pass Empty for (s)
    -- we will pass Text for error message (e) which is better to use than String for large amounts of data
    -- we don't need to produce an interesting result so we return a Unit () for (a)
myContract1 :: Contract () Empty Text ()
myContract1 = do
    -- here we throw an error an then toss the resulting type to return a Unit 
    void $ Contract.throwError "BOOM!" -- <- this line causes the program to stop and the contract doesn't continue execution
    -- Here we tell the compiler that this is a String since we are using TypeApplication and OverloadedStrings
    Contract.logInfo @String "hello from the contract"

-- this is to test the contract above
    -- we simply activate the contract using activateContractWallet on the address from wallet 1 and then passing in myContract1
myTrace1 :: EmulatorTrace ()
-- void is used to throw the result away so we can return out the unit we want from EmulatorTrace
myTrace1 = void $ activateContractWallet (knownWallet 1) myContract1

-- here we run our trace emulation with IO using runEmulatorTraceIO
test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1

-- This example we will handle the exeption with a catch 
    -- once caught we will pass along the call to myContract1
-- we now pass through an error type of void
    -- Void has no values instead of Unit with just one
    -- this means this contract can't possibly throw an execption
myContract2 :: Contract () Empty Void ()
-- handleError takes a handle to replace the error with
    -- in this example we use lambda syntax to make the handleError be a new string saying we caught some error
    -- then we pass the contract function with the error we are handling
myContract2 = Contract.handleError
    (\err -> Contract.logError $ "caught: " ++ unpack err)
    myContract1

-- same trace as above but called on myContract2
myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (knownWallet 1) myContract2

-- again mimicking above test but for myTrace2
test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2

-- this is a custom schema with endpoints for use below in parameter (s) i.e. schema 
    -- typically a schema contains the name schema
    -- (Endpoint) then 
        -- the name of the endpoint then
            -- the type of the endpoint
                -- (.\/) type operator used for combining types in schemas and other things 
                -- need to add {-# LANGUAGE TypeOperators     #-} at top 
type MySchema = Endpoint "foo" Int .\/ Endpoint "bar" String

-- here we mimic the first example but instead use the Schema defined above
myContract3 :: Contract () MySchema Text ()
myContract3 = do
    -- we awaitPromise to be returned from the endpoint call of each endpoint
        -- we use Haskell type application and pass Contract.logInfo to convert from Int to Contract
    awaitPromise $ endpoint @"foo" Contract.logInfo
    awaitPromise $ endpoint @"bar" Contract.logInfo

myTrace3 :: EmulatorTrace ()
myTrace3 = do
    -- we now bind to the contract wallet and we use it when we call callEndpoint
        -- it takes three params
            -- the endpoint
            -- the contract handle
            -- the value for the endpoint
    h <- activateContractWallet (knownWallet 1) myContract3
    callEndpoint @"foo" h 42
    callEndpoint @"bar" h "Haskell"

test3 :: IO ()
test3 = runEmulatorTraceIO myTrace3

-- important stuff from the trace output
{-
  Receive endpoint call on 'foo' for Object (fromList [("contents",Array [Object (fromList [("getEndpointDescription",String "foo")]),Object (fromList [("unEndpointValue",Number 42.0)])]),("tag",String "ExposeEndpointResp")])
Slot 00001: 00000000-0000-4000-8000-000000000000 {Wallet W872c}:
  Contract log: Number 42.0
Slot 00001: 00000000-0000-4000-8000-000000000000 {Wallet W872c}:
  Receive endpoint call on 'bar' for Object (fromList [("contents",Array [Object (fromList [("getEndpointDescription",String "bar")]),Object (fromList [("unEndpointValue",String "Haskell")])]),("tag",String "ExposeEndpointResp")])
Slot 00001: *** CONTRACT LOG: "Haskell"
Slot 00001: 00000000-0000-4000-8000-000000000000 {Wallet W872c}:
  Contract instance stopped (no errors)
-}

-- now we will look at an example to make use of the first parameter 
    -- the (w) argument must be an instance of type Monoid
        -- has mempty and mappend
            -- mempty is like a neutral element 
            -- mappend appends an element to the end
        -- for a list mempty would be [] and mappend would be concat
            -- mempty :: [Int] == []
            -- mappend [1,2,3::Int] [4,5,6] == [1,2,3,4,5,6]
-- here we will use a list of ints
-- Empty for endpoint schema 
-- Text for Errors
-- Unit for result
myContract4 :: Contract [Int] Empty Text ()
myContract4 = do
    -- first we wait 10 slots 
        -- throw the bulk away
    void $ Contract.waitNSlots 10
    -- make use of the Writer feature with tell
        -- takes one argument 
    tell [1]
    -- ten more slots waiting
    void $ Contract.waitNSlots 10
    -- another tell
    tell [2]
    -- another wait
    void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()
myTrace4 = do
    -- in this trace w bind the contract handle to h again like before
    h <- activateContractWallet (knownWallet 1) myContract4

    -- here we use observableState on the contractHandle and bind it to xs
        -- observableState returns the current state of a contract
    -- this first log of the state occurs before any calls to (tell) occur
    void $ Emulator.waitNSlots 5
    xs <- observableState h
    Extras.logInfo $ show xs

    -- here we do the same lookup of the contract state but at slot 10 before the second call to (tell) occurs
    void $ Emulator.waitNSlots 10
    ys <- observableState h
    Extras.logInfo $ show ys

    -- this check occurs after the second call to (tell) and after the second waitNSlots call
    void $ Emulator.waitNSlots 10
    zs <- observableState h
    Extras.logInfo $ show zs

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4
{-
Slot 00007: *** USER LOG: []
Slot 00017: *** USER LOG: [1]
Slot 00027: *** USER LOG: [1,2]
-}

-- This is useful for logging to outside world for state management