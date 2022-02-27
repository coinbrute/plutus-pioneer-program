{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Spec.Trace
    ( tests
    , runMyTrace
    ) where

import           Control.Lens
import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Ledger
import           Ledger.Value
import           Ledger.Ada                 as Ada
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, String, Show (..))
import           Test.Tasty

import           Week8.TokenSale

-- checkPredicateOptions is a Tasty TestTree function 
tests :: TestTree
tests = checkPredicateOptions
    -- we modify the emulatorConfig with our own to use our starting values and our wallet designations
        -- the defaultCheckOptions are the default options to use for the testing scenario
        -- we use a lens from checkOptions to our emulator config setting the default one to our own
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "token sale trace"
    -- walletFundsChange checks that the funds in the wallet have changed by the given amount excluding fees
        -- so wee take wallet x and the lovelaceValue of n and the token value of m
    (     walletFundsChange (Wallet 1) (Ada.lovelaceValueOf   10_000_000  <> assetClassValue token (-60))
     .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf (-20_000_000) <> assetClassValue token   20)
     .&&. walletFundsChange (Wallet 3) (Ada.lovelaceValueOf (- 5_000_000) <> assetClassValue token    5)
    )
    -- then we add our trace
    myTrace

-- run the trace using the config prived and the default one on top 
runMyTrace :: IO ()
runMyTrace = runEmulatorTraceIO' def emCfg myTrace

-- wallet configs for starting off 
emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(Wallet w, v) | w <- [1 .. 3]]) def def
  where
    -- value of tokens to give to each wallet starting off 
    v :: Value
    v = Ada.lovelaceValueOf 1_000_000_000 <> assetClassValue token 1000

currency :: CurrencySymbol
currency = "aa"

name :: TokenName
name = "A"

token :: AssetClass
token = AssetClass (currency, name)

myTrace :: EmulatorTrace ()
myTrace = do
    h <- activateContractWallet (Wallet 1) startEndpoint
    -- give our currency and name from above and True so we use our ThreadToken and call the Start endpoint
    callEndpoint @"start" h (currency, name, True)
    -- wiat 5 slots
    void $ Emulator.waitNSlots 5
    -- check observable state which should contain the nft
    Last m <- observableState h
    case m of
        Nothing -> Extras.logError @String "error starting token sale"
        -- we found the token sale
        Just ts -> do
            Extras.logInfo $ "started token sale " ++ show ts
            -- activate each wallet on the use endpoint using the utxo for the token sale
            h1 <- activateContractWallet (Wallet 1) $ useEndpoints ts
            h2 <- activateContractWallet (Wallet 2) $ useEndpoints ts
            h3 <- activateContractWallet (Wallet 3) $ useEndpoints ts

            -- call set price to 1 ada with wallet 1 and wait 5 slots
            callEndpoint @"set price" h1 1_000_000
            void $ Emulator.waitNSlots 5

            -- call add tokens with wallet 1 and wait 5 slots
            callEndpoint @"add tokens" h1 100
            void $ Emulator.waitNSlots 5

            -- call buy tokens for 20 tokens with wallet 2 and wait 5 slots
            callEndpoint @"buy tokens" h2 20
            void $ Emulator.waitNSlots 5

            -- call buy tokens for 5 tokens with wallet 3 and wait 5 slots
            callEndpoint @"buy tokens" h3 5
            void $ Emulator.waitNSlots 5

            -- call withdraw for (40 tokens, 10 ada) with wallet 1 and wait 5 slots
            callEndpoint @"withdraw" h1 (40, 10_000_000)
            void $ Emulator.waitNSlots 5
