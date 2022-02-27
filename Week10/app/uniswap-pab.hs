{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Main
    ( main
    ) where

import           Control.Monad                       (forM_, void)
import           Control.Monad.Freer                 (interpret)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (Result (..), encode, fromJSON)
import qualified Data.ByteString.Lazy                as LB
import           Data.Default                        (Default (..))
import qualified Data.Monoid                         as Monoid
import qualified Data.Semigroup                      as Semigroup
import           Data.Text                           (Text)
import qualified Plutus.Contracts.Currency           as Currency
import qualified Plutus.Contracts.Uniswap            as Uniswap
import           Plutus.PAB.Effects.Contract.Builtin (Builtin)
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers, logString)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Prelude                             hiding (init)
import           Wallet.Emulator.Types               (Wallet (..))
import           Wallet.Types                        (ContractInstanceId (..))

import           Uniswap                             as US

-- pab for the uniswap end to end walkthrough

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    -- start the pab server with a handler and get the handler to shut down
    logString @(Builtin UniswapContracts) "Starting Uniswap PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug
    -- wallet 1 activates init contract 
        -- this mints the 4 million a/b/c/d tokens and distributes them to each wallet from wallet 1
    cidInit  <- Simulator.activateContract (Wallet 1) Init
    -- here we wait until init returns then we write the currency symbol for the forged tokens into state
    cs       <- flip Simulator.waitForState cidInit $ \json -> case fromJSON json of
                    Success (Just (Semigroup.Last cur)) -> Just $ Currency.currencySymbol cur
                    _                                   -> Nothing
    _        <- Simulator.waitUntilFinished cidInit

    -- then we write the new cs and encode it to json and write to file
    liftIO $ LB.writeFile "symbol.json" $ encode cs
    logString @(Builtin UniswapContracts) $ "Initialization finished. Minted: " ++ show cs

    -- then again on wallet 1 we start the uniswap constructor
    cidStart <- Simulator.activateContract (Wallet 1) UniswapStart
    -- wait for the result since we need it to parameterize the user contracts
    us       <- flip Simulator.waitForState cidStart $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text Uniswap.Uniswap))) of
                    Success (Monoid.Last (Just (Right us))) -> Just us
                    _                                       -> Nothing
    logString @(Builtin UniswapContracts) $ "Uniswap instance created: " ++ show us

    -- now we start the user instances looping on all wallets parameterizing with the (us) from previous step
    forM_ wallets $ \w -> do
        cid <- Simulator.activateContract w $ UniswapUser us
        -- lift the handle from the contractInstance and write to file so we can interact with them 
        liftIO $ writeFile (cidFile w) $ show $ unContractInstanceId cid
        logString @(Builtin UniswapContracts) $ "Uniswap user contract started for " ++ show w

    -- wait until user types a key and we can shutdown
    void $ liftIO getLine

    shutdown

handlers :: SimulatorEffectHandlers (Builtin UniswapContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin UniswapContracts) def def
    $ interpret
    $ Builtin.contractHandler
    $ Builtin.handleBuiltin @UniswapContracts
