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

module Week6.Oracle.Test where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Data.Text                  (Text)
import           Ledger
import           Ledger.Value               as Value
import           Ledger.Ada                 as Ada
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (IO, Semigroup(..), Show (..))
import           Wallet.Emulator.Wallet

import           Week6.Oracle.Core
import           Week6.Oracle.Funds
import           Week6.Oracle.Swap

-- type synonym for CurrencySymbol
assetSymbol :: CurrencySymbol
assetSymbol = "ff"

-- type synonym for TokenName
assetToken :: TokenName
assetToken = "USDT"

-- here we define a test to run an emulator with a little bit different version then the other times we use runEmulatorTraceIO 
  -- this time it takes extra parameters to be more specific with configuration
    -- the first param is the specific configuration we are prividing to the emulator on top of any default configuration specified
      -- by specifying (def) we denote the use of the default config with our emCfg extra bits on top
    -- the second isthe trace to run
test :: IO ()
test = runEmulatorTraceIO' def emCfg def myTrace
  where
    -- here we specify our wallet config for the above emCfg param
    emCfg :: EmulatorConfig
    -- we use list comprehension to create ten wallets  and specify the keys to be 1-10 and the values to be 100 ada i.e 100 million lovelace
    emCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet i, v) | i <- [1 .. 10]]

    -- here is where make the values for those wallets 
      -- we get the lovelaceValueOf 100 million 
        -- and then create a singleton Value with our assetSymbol and assetToken using 100 million as the amount
    v :: Value
    v = Ada.lovelaceValueOf                    100_000_000 <>
        Value.singleton assetSymbol assetToken 100_000_000

-- here is a simple function that will run constantly to check the value of the oracle contract and log out a message 
checkOracle :: Oracle -> Contract () Empty Text a
checkOracle oracle = do
    -- first we call findOracle from Core module with the Oracle passed in and bind the resulting triple to m
    m <- findOracle oracle
    case m of
        -- if Nothing is found we return a Unit and stop execution since we don't have a unit to work with
        Nothing        -> return ()
        -- else we just grab out the amount and log the message
        Just (_, _, x) -> Contract.logInfo $ "Oracle value: " ++ show x
    -- wait 1 slot and call the function again 
    Contract.waitNSlots 1 >> checkOracle oracle

-- this is the trace to be run with the test above
myTrace :: EmulatorTrace ()
myTrace = do
    -- specify the params for the Oracle 
    let op = OracleParams
                { opFees = 1_000_000 -- fees for the oracle
                , opSymbol = assetSymbol -- symbol for asset class
                , opToken  = assetToken -- token for asset class
                }
    -- start the oracle for wallet 1
    h1 <- activateContractWallet (Wallet 1) $ runOracle op
    -- wait 1 slot
    void $ Emulator.waitNSlots 1
    -- get the oracle with wallet 1
    oracle <- getOracle h1
    -- check the oracle then activate the contract on wallet 2
    void $ activateContractWallet (Wallet 2) $ checkOracle oracle

    -- call update with wallet 1 using 1.5 ada as value
      -- this means the exchange rate is 1.5 now
    callEndpoint @"update" h1 1_500_000
    -- wait 3 slots
    void $ Emulator.waitNSlots 3

    -- activate ownFunds' contract function on wallets 1/3/4/5
      -- this is to check initial balances
    void $ activateContractWallet (Wallet 1) ownFunds'
    void $ activateContractWallet (Wallet 3) ownFunds'
    void $ activateContractWallet (Wallet 4) ownFunds'
    void $ activateContractWallet (Wallet 5) ownFunds'

    -- now we activate swap contract on wallets 3/4/5
    h3 <- activateContractWallet (Wallet 3) $ swap oracle
    h4 <- activateContractWallet (Wallet 4) $ swap oracle
    h5 <- activateContractWallet (Wallet 5) $ swap oracle

    -- wallets 3 and 4 call endpoint offer for 10 and 20 ADA respectively
    callEndpoint @"offer" h3 10_000_000
    callEndpoint @"offer" h4 20_000_000
    -- wait 3 slots
    void $ Emulator.waitNSlots 3

    -- wallet 5 uses the swap
      -- it will take whichever it finds first
    callEndpoint @"use" h5 ()
    -- wait 3 slots
    void $ Emulator.waitNSlots 3

    -- wallet 1 calls update with 1.7 ADA as value
      -- exchange is now at 1.7
    callEndpoint @"update" h1 1_700_000
    -- wait 3 slots
    void $ Emulator.waitNSlots 3

    -- wallet 5 uses swap
    callEndpoint @"use" h5 ()
    -- wait 3 slots
    void $ Emulator.waitNSlots 3

    -- wallet 1 updates oracle value to 1.8 again
    callEndpoint @"update" h1 1_800_000
    -- wait 3 slots
    void $ Emulator.waitNSlots 3

    -- wallet 3 and 4 call retrieve
    callEndpoint @"retrieve" h3 ()
    callEndpoint @"retrieve" h4 ()
    -- wait 3 slots
    void $ Emulator.waitNSlots 3
  where
    -- this function retrieves an oracle with a handle
    getOracle :: ContractHandle (Last Oracle) OracleSchema Text -> EmulatorTrace Oracle
    getOracle h = do
        -- get the observable state of the oracle contract handle 
        l <- observableState h
        case l of
            Last Nothing       -> Emulator.waitNSlots 1 >> getOracle h
            -- log for debugging and return the oracle
            Last (Just oracle) -> Extras.logInfo (show oracle) >> return oracle
