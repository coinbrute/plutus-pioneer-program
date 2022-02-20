{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week03.Homework2 where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String, undefined)
import           Text.Printf          (printf)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# INLINABLE mkValidator #-}
mkValidator :: PaymentPubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkValidator pkh t () ctx = traceIfFalse "Beneficiary's signature is missing." signedByBenificiary && traceIfFalse "Deadline has not been reached" deadlineReached
    where 
        info :: TxInfo
        info = scriptContextTxInfo ctx

        -- is pkh an element of the tx signatories from info
        signedByBeneficiary :: Bool
        signedByBeneficiary = unPaymentPubKeyHash pkh `elem` txInfoSignatories info

        -- is s contained within the valid tx range from info
        deadlineReached :: Bool
        deadlineReached = from s `contains` txInfoValidRange info

data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = POSIXTime
    type instance RedeemerType Vesting = ()

-- here similar to Parametarized.hs we are lifting out the PatmentPbuKeyHash code and compiling it from haskell to plutus in the same roundabout way we did before just with a different type
typedValidator :: PaymentPubKeyHash -> Scripts.TypedValidator Vesting
typedValidator p = Scripts.mkTypedValidator @Vesting ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p) $$(PlutusTx.compile [|| wrap ||])
    where -- we use POSIXTime for the datum
        wrap = Scripts.wrapValidator @POSIXTime @()

validator :: PaymentPubKeyHash -> Validator
validator = Scripts.validatorScript . typedValidator

scrAddress :: PaymentPubKeyHash -> Ledger.Address
scrAddress = Scripts.validatorHash . typedValidator

data GiveParams = GiveParams
    { gpBeneficiary :: !PaymentPubKeyHash
    , gpDeadline    :: !POSIXTime
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
            Endpoint "give" GiveParams
        .\/ Endpoint "grab" ()

give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
    let p  = gpBeneficiary gp
        d  = gpDeadline gp
        tx = Constraints.mustPayToTheScript d $ Ada.lovelaceValueOf $ gpAmount gp
    ledgerTx <- submitTxConstraints (typedValidator p) tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    now   <- currentTime
    pkh   <- ownPaymentPubKeyHash
    utxos <- Map.filter (isSuitable now) <$> utxosAt (scrAddress pkh)
    if Map.null utxos
        then logInfo @String $ "no gifts available"
        else do
            let orefs   = fst <$> Map.toList utxos
                lookups = Constraints.unspentOutputs utxos        <>
                          Constraints.otherScript (validator pkh)
                tx :: TxConstraints Void Void
                tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
                          Constraints.mustValidateIn (from now)
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "collected gifts"
  where
    isSuitable :: POSIXTime -> ChainIndexTxOut -> Bool
    isSuitable now o = case _ciTxOutDatum o of
        Left _          -> False
        Right (Datum e) -> case PlutusTx.fromBuiltinData e of
            Nothing -> False
            Just d  -> d <= now

endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" $ const grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []
