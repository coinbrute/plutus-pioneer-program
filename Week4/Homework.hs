{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Week4.Homework where

import Data.Aeson             (FromJSON, ToJSON)
import Data.Functor           (void)
import Data.Text              (Text, unpack)
import GHC.Generics           (Generic)
import Ledger
import Ledger.Ada             as Ada
import Ledger.Constraints     as Constraints
import Plutus.Contract        as Contract
import Plutus.Trace.Emulator  as Emulator
import Wallet.Emulator.Wallet

data PayParams = PayParams
    { ppRecipient :: PaymentPubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

-- make a payment to pubkeyaddress in amount specified
payContract :: Contract () PaySchema Text ()
payContract = do
    -- this calls the endpoint "pay" and returns the value of the params
        -- it awaits the promis and binds the result to (pp)
    pp <- awaitPromise $ endpoint @"pay" return
    -- establishes tx by extracting the rexipient and lovelace fields retrieved from the endpoint promise resolved above
        -- recipient resolved through call to mustPayToPubKey
        -- lovelace value extrated than passed to lovelaceValueOf to get convert the value 
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    -- sumbmit the tx and toss the result
        -- then pass that into the handleError function in case there were any missteps along the way
        -- this way they will be handled and the contract wont crash
    handleError (\err -> Contract.logInfo $ "caught error: " ++ unpack err) $ void $ submitTx tx
    -- recursively call the contract to keep calling pay endpoint
    payContract

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace x y = do
    -- first we need to bind the payer wallet to the contract handle being used throughout 
        -- we do that the same we we have done previously using knownWallet on 1 and then passing in the contract to use on activateContractWallet
    h <- activateContractWallet (knownWallet 1) payContract
    -- now we also need wallet 2 for the PaymentPubKyHash in the PayParams
        -- we get that the same way we have before in the repl
    let pkh = mockWalletPaymentPubKeyHash $ knownWallet 2
    -- call the endpoint pay with the wallet 2 pkh and the x value for the amount
    callEndpoint @"pay" h $ PayParams
        { ppRecipient = pkh
        , ppLovelace  = x
        }
    -- wait 1 slot
    void $ Emulator.waitNSlots 1
    callEndpoint @"pay" h $ PayParams
        { ppRecipient = pkh
        , ppLovelace  = y 
        }
    -- wait another slot
    void $ Emulator.waitNSlots 1



payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 10_000_000 20_000_000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000_000_000 20_000_000
