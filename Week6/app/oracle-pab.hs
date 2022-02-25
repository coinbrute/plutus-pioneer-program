{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Main
    ( main
    ) where

import           Control.Monad                       (forM_, void, when)
import           Control.Monad.Freer                 (Eff, Member, interpret, type (~>))
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.Freer.Extras.Log      (LogMsg)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON, Result (..), fromJSON)
import           Data.Default                        (Default (..))
import           Data.Monoid                         (Last (..))
import           Data.Text                           (Text, pack)
import           Ledger
import           Ledger.Constraints
import qualified Ledger.Value                        as Value
import           Plutus.Contract
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), endpointsToSchemas, handleBuiltin)
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import qualified Plutus.Contracts.Currency           as Currency

import           Wallet.Emulator.Types               (Wallet (..), walletPubKey)
import           Wallet.Types                        (ContractInstanceId (..))

import qualified Week6.Oracle.Core                  as Oracle
import           Week6.Oracle.PAB                   (OracleContracts (..))
import qualified Week6.Oracle.Swap                  as Oracle

-- this is a proper main so it can be an executable
    -- uses the Simulator monad which is similar to the Emulator trace
    -- you can start stop contract 
    -- you can inspect the state
    -- you can call endpoints
    -- main difference is with Simulator you can have IO side effects
main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    -- log message that server is starting
    Simulator.logString @(Builtin OracleContracts) "Starting Oracle PAB webserver. Press enter to exit."
    -- start the server binding the return to be able to shut it down later
    shutdown <- PAB.Server.startServerDebug

    -- activate Init contract for wallet 1
        -- bind to cidInit
    cidInit <- Simulator.activateContract (Wallet 1) Init
    -- get the currency symbol from the init contract 
        -- waitForLast waits for the initilized status of the contract before attempting to bind the currency symbol 
    cs      <- waitForLast cidInit
    -- then we wait until the init contract has finished
    _       <- Simulator.waitUntilFinished cidInit

    -- now we run the oracle contract and activate it on wallet 1 binding it to cidOracle
    cidOracle <- Simulator.activateContract (Wallet 1) $ Oracle cs
    -- liftIO lifts the IO action out into the Simulator Monad
        -- here we need a way to talk to the contract later so we write to file after gettin gthe instance of the contract handle
    liftIO $ writeFile "oracle.cid" $ show $ unContractInstanceId cidOracle
    -- use waitForLast to get the oracle again
    oracle <- waitForLast cidOracle

    -- looking over all the wallets except 1 activate the swap contract with the oracle 
    forM_ wallets $ \w ->
        when (w /= Wallet 1) $ do
            -- bind the contract handle
            cid <- Simulator.activateContract w $ Swap oracle
            -- like with the oracle contract bind the to a cid file using the wallet as a signifier
                -- lift the IO action out into Simulator
            liftIO $ writeFile ('W' : show (getWallet w) ++ ".cid") $ show $ unContractInstanceId cid

    -- use liftIO on getLine to block until enter is pressed
    void $ liftIO getLine
    -- then shutdown
    shutdown

-- with contract provided we can return a Simlation 
waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLast cid =
    -- read and serialie the state of the contract 
        -- provide the json value to a predicate and then wait until we get a Just value 
        -- this indicates we have state and the contract is initialized
    flip Simulator.waitForState cid $ \json -> case fromJSON json of
        -- Success is from Data.Aeson used to handle the json data
        Success (Last (Just x)) -> Just x
        -- if the parsing fails we return nothing
        _                       -> Nothing

-- list of wallets 
wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 5]]

-- Token name for oracle usdt
usdt :: TokenName
usdt = "USDT"

-- parameters for oracle
oracleParams :: CurrencySymbol -> Oracle.OracleParams
oracleParams cs = Oracle.OracleParams
    { Oracle.opFees   = 1_000_000
    , Oracle.opSymbol = cs
    , Oracle.opToken  = usdt
    }

-- this is boiler plate to hook up the data type defined with actual contracts
handleOracleContracts ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin OracleContracts))) effs
    )
    => ContractEffect (Builtin OracleContracts)
    ~> Eff effs
handleOracleContracts = handleBuiltin getSchema getContract where
    -- this matches with the schema
    getSchema = \case
        Init     -> endpointsToSchemas @Empty -- init wont have a schema 
        Oracle _ -> endpointsToSchemas @Oracle.OracleSchema
        Swap _   -> endpointsToSchemas @Oracle.SwapSchema
    -- this matches with the contract
    getContract = \case
        Init        -> SomeBuiltin   initContract -- init runs the init contract
        Oracle cs   -> SomeBuiltin $ Oracle.runOracle $ oracleParams cs -- oracle runs runOracle
        Swap oracle -> SomeBuiltin $ Oracle.swap oracle -- swap runs the swap

-- more boilerplate 
handlers :: SimulatorEffectHandlers (Builtin OracleContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin OracleContracts) def []
    $ interpret handleOracleContracts

-- here we init the starter funds
initContract :: Contract (Last CurrencySymbol) Empty Text ()
initContract = do
    -- get our own public key
    ownPK <- pubKeyHash <$> ownPubKey
    cur   <-
        mapError (pack . show)
        -- mint amount needed in usdt for each wallet
        (Currency.mintContract ownPK [(usdt, fromIntegral (length wallets) * amount)]
        :: Contract (Last CurrencySymbol) Empty Currency.CurrencyError Currency.OneShotCurrency)
    -- get the currency symbol
    let cs = Currency.currencySymbol cur
    -- create a singleton of the value using currency symbol usdt token name and amount
        v  = Value.singleton cs usdt amount
    -- 
    forM_ wallets $ \w -> do
        -- get the pkh from w in wallets
        let pkh = pubKeyHash $ walletPubKey w
        -- any time the pkh isn't your own do the following
        when (pkh /= ownPK) $ do
            -- submit a tx to pay to the pkh provided the value decided above from (v)
            tx <- submitTx $ mustPayToPubKey pkh v
            -- await confirmation
            awaitTxConfirmed $ txId tx
    -- tell the currency symbol we just minted
    tell $ Last $ Just cs
  where
    -- amount is an integer of 100 million lovelace
    amount :: Integer
    amount = 100_000_000
