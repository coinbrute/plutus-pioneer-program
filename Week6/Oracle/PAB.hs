{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}

module Week6.Oracle.PAB
    ( OracleContracts (..)
    ) where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import           GHC.Generics              (Generic)
import           Ledger

import qualified Week6.Oracle.Core        as Oracle

-- the focus of this module is to be able to look at the Plutus application backend
    -- this the ability to take the plutus code and package it in an executable as a dApp

-- this type def redefines the contract definition we want to run
    -- Init setups an environment where tokens, wallets and funds are available
    -- Oracle is the onstructor for the runOracle contract and provide the update endpoint
        -- the CurrencySymbol will communicate the symbol used for our us dollar token
    -- the Swap parameterized by Oracle will run the Swap contract
data OracleContracts = Init | Oracle CurrencySymbol | Swap Oracle.Oracle
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

-- we make this an instance of Pretty
instance Pretty OracleContracts where
    pretty = viaShow
