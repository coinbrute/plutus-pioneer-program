{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week02.Typed where

import           Control.Monad        hiding (fmap)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins    as Builtins
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), String)
import           Text.Printf          (printf)

{-# INLINABLE mkValidator #-}
-- now we are using specific types for our datum redeemer and script
    -- the datum doesn't much matter here so we just use a unit
    -- we will use an integer for the redeemer
    -- script will be scriptContext
    -- then we return a true if the validation is good 
        -- we can still throw errors though
mkValidator :: () -> Integer -> ScriptContext -> Bool
-- here we say just look at the redeemer and take the output of r == 42 into the traceIfFalse 
mkValidator _ r _ = traceIfFalse "wrong redeemer" $ r == 42

-- this type encodes the information we need from the datum or the redeemer
    -- we say the datum is the type we use above as well as the redeemer
data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = ()
    type instance RedeemerType Typed = Integer

-- we now use the typed version of the scrips and then call mkTypedValidator on the new Typed type 
    -- also need to convert to the new types in the compilation with the (wrap) function
        -- use the where predicate with wrapValidator on the types for the wrap function
    
typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @Integer

-- this is the same as low level validation scripts
    -- the addtion being the call to typedValidator
validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator


type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" Integer

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    -- this call now is made to a specific script since it is made ot a specific redeemer script
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()
grab r = do
    utxos <- utxosAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI r | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" grab

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []
