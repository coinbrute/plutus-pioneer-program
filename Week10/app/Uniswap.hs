{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Uniswap where

import           Control.Monad                       (forM_, when)
import           Data.Aeson                          (FromJSON, ToJSON)
import qualified Data.Semigroup                      as Semigroup
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)
import           Ledger
import           Ledger.Constraints
import           Ledger.Value                        as Value
import           Plutus.Contract
import qualified Plutus.Contracts.Currency           as Currency
import qualified Plutus.Contracts.Uniswap            as Uniswap
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Wallet.Emulator.Types               (Wallet (..), walletPubKey)

-- tweaked clone of the uniswap contract from plutus

data UniswapContracts =
      Init -- used to create some example tokens and distribute them in the beginning
    | UniswapStart -- the uniswap start schema for setting it all up for the owner
    | UniswapUser Uniswap.Uniswap -- parameterized by the Uniswap which is returned by the start call and this is used by the users of the swap
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty UniswapContracts where
    pretty = viaShow

-- designate the line between the schema and the type of schema and the contract and the endpoints parameters
instance Builtin.HasDefinitions UniswapContracts where
    getDefinitions = [Init, UniswapStart]
    getSchema = \case
        UniswapUser _ -> Builtin.endpointsToSchemas @Uniswap.UniswapUserSchema
        UniswapStart  -> Builtin.endpointsToSchemas @Uniswap.UniswapOwnerSchema
        Init          -> Builtin.endpointsToSchemas @Empty
    getContract = \case
        UniswapUser us -> Builtin.SomeBuiltin $ Uniswap.userEndpoints us
        UniswapStart   -> Builtin.SomeBuiltin Uniswap.ownerEndpoint
        Init           -> Builtin.SomeBuiltin initContract

-- here we init the contract and it sets up the initial funds
    -- we forge the tokens to one wallet then send the designated amount to the other wallets
initContract :: Contract (Maybe (Semigroup.Last Currency.OneShotCurrency)) Currency.CurrencySchema Currency.CurrencyError ()
initContract = do
    ownPK <- pubKeyHash <$> ownPubKey
    cur   <- Currency.mintContract ownPK [(tn, fromIntegral (length wallets) * amount) | tn <- tokenNames]
    let cs = Currency.currencySymbol cur
        v  = mconcat [Value.singleton cs tn amount | tn <- tokenNames]
    forM_ wallets $ \w -> do
        let pkh = pubKeyHash $ walletPubKey w
        when (pkh /= ownPK) $ do
            tx <- submitTx $ mustPayToPubKey pkh v
            awaitTxConfirmed $ txId tx
    tell $ Just $ Semigroup.Last cur
  where
    amount = 1000000

-- helper functions 
wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 4]]

tokenNames :: [TokenName]
tokenNames = ["A", "B", "C", "D"]

-- designate file name for given wallet
cidFile :: Wallet -> FilePath
cidFile w = "W" ++ show (getWallet w) ++ ".cid"
