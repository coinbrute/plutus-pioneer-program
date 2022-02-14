{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week02.FortyTwo where

import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Plutus.Contract
import           PlutusTx            (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins   as Builtins
import           PlutusTx.Prelude    hiding (Semigroup(..), unless)
import           Ledger              hiding (singleton)
import           Ledger.Constraints  as Constraints
import qualified Ledger.Scripts      as Scripts
import           Ledger.Ada          as Ada
import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))
import           Prelude             (IO, Semigroup (..), String)
import           Text.Printf         (printf)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# INLINABLE mkValidator #-}
-- in this case this validator script cares about a spcific redeemer. For this example it should be equal to the Integer 42 which we create using Plutus Builtins make Integer function (mkI). if it is valid then we return a unit else return an error
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ r _
    | r == Builtins.mkI 42 = ()
    | otherwise            = traceError "wrong redeemer!"

-- compile the validator
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

-- create the validator hash using the compiled validator
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

-- create a script address for the script using the validator
scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

-- the two endpoints now get an Integer
type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" Integer

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()
grab n = do
    utxos <- utxosAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        --  while before we used an arbitrary value for the redeemer mkI call now we pass in the output from n
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI n | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    -- we also directly pass in grab as the constructor now
    grab' = endpoint @"grab" grab

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []
