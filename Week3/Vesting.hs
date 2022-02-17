{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week03.Vesting where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (Data (..))
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
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)


-- essentially this script allows for money to be put into a wallet then allows for someone else to pull it out only after a certain amount of time. 
-- the validator uses checks for both time constraints and correct signature information by the benficiary
-- the PaymentPubKeyHash is used to be the signature for the beneficiary
-- we use POSIXTime to represent the deadline that must be met in order to unlock the script for validation


data VestingDatum = VestingDatum
    { beneficiary :: PaymentPubKeyHash
    , deadline    :: POSIXTime
    } deriving Show

-- again we use template haskell here to avoid the writing out of data types 
PlutusTx.unstableMakeIsData ''VestingDatum

-- use the custom VestingDatum type above for the datum
-- we dont need any additional info from the redeemer since its all in the datum with the beneficiary signature and deadline info so we pass in a unit for the redeemer
-- we then pass in a ScriptContext for the script since this time we actually are looking at it
{-# INLINABLE mkValidator #-}
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
-- this time we are passing a boolean on both the condition that the beneficiary signed the datum and the deadline was reached
mkValidator dat () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                         traceIfFalse "deadline not reached" deadlineReached
-- now we define the helpers in the where predicate
  where
    -- grab the TxInfo from the ScriptContext
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- get the beneficiary value from the datum
    -- pass that to txSignedBy as the second argument 
    -- call txSignedBy on the info we grab above 
    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ beneficiary dat

    -- first check the valid range for the tx on the TxInfo
    -- pass that as the second arg for contains 
    -- call from on the deadline in our datum to get an interval back
    -- then see if it is contained in the valid range for the TxInfo
    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info

-- create instance of this validator script with the Datum and Redeemer 
data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = VestingDatum
    type instance RedeemerType Vesting = ()

-- Plutus compiler validator boiler plate
typedValidator :: Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VestingDatum @()

-- get the validator
validator :: Validator
validator = Scripts.validatorScript typedValidator

-- calculate the hash
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

-- get the script address
scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

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
    let dat = VestingDatum
                { beneficiary = gpBeneficiary gp
                , deadline    = gpDeadline gp
                }
        tx  = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ gpAmount gp
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    now   <- currentTime
    pkh   <- ownPaymentPubKeyHash
    -- filter the utxos where I am the beneficiaries and deadline is reached
    utxos <- Map.filter (isSuitable pkh now) <$> utxosAt scrAddress
    if Map.null utxos
        then logInfo @String $ "no gifts available"
        -- create transaction with utxos from above
        else do
            -- get the refs to the utxos
            let orefs   = fst <$> Map.toList utxos
                -- create lookup with utxos and validator using function combine (<>)
                lookups = Constraints.unspentOutputs utxos  <>
                          Constraints.otherScript validator
                -- for tx constraints we want to spend the output of the given refs by looking for suitable ones and saying we want to spend those by looking at a unit redeemer and by looking that the validation is after now
                tx :: TxConstraints Void Void
                tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
                          Constraints.mustValidateIn (from now)
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "collected gifts"
  where
    isSuitable :: PaymentPubKeyHash -> POSIXTime -> ChainIndexTxOut -> Bool
    -- check the datum key hash
    isSuitable pkh now o = case _ciTxOutDatum o of
        -- if there is no hash pass false
        Left _          -> False
        -- if there is a datum check for a hash
        Right (Datum e) -> case PlutusTx.fromBuiltinData e of
            -- if there is no suitable hash pass false
            Nothing -> False
            -- else check if the beneficiary hash equals the hash in the datum and the deadline in the datum is lT or eq now
            Just d  -> beneficiary d == pkh && deadline d <= now

endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" $ const grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []
