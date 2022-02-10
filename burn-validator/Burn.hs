{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-} -- no ghc prelude use the imported plutus one instead
{-# LANGUAGE OverloadedStrings   #-} -- convertes Haskell Strings to Plutus Bytestrings
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week02.Burn where

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

{-# INLINABLE mkValidator #-} -- this allows for functions to be inserted into the template haskell portion for compilation to plutus core
-- function to make the validator for the transaction
-- gets three pieces of info datum redeemer and context
-- datum from output
-- redeemer from input
-- context is consuming transaction
-- () is a type unit similar to type void in imperative languages 
-- in this case we are returning out a special error function that takes the () "unit" as a parameter.
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = traceError "BURNT!" -- always fails no matter what the input
-- this basically would be a validator saying whatever ada is passed in will never become available to anyone else

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||]) -- uses template haskell to compile
-- [|| ||] these allow access to the underlying code stream within the mkValidator function for the compile function
-- $$ this syntax takes the syntax tree and splices into the source code at that point. 
-- PlutusTx.compile [|| mkValidator ||] gives us the plutus core compiled code
-- add $$() before compilation we get the spliced in code and turned into a validator

valHash :: Ledger.ValidatorHash -- hash of the validator
valHash = Scripts.validatorHash validator

srcAddress :: Ledger.Address
srcAddress = scriptAddress validator

type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" ()

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkConstr 0 []) $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    utxos <- utxosAt srcAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI 17 | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" $ const grab

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []
