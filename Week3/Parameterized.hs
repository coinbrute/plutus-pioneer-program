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

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week03.Parameterized where

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

-- the additional type for paramterized validation instead of a datum is now a paramaratized object called VestingParam 
-- we pass this before the datum object in the mkValidator call 
data VestingParam = VestingParam
    { beneficiary :: PaymentPubKeyHash
    , deadline    :: POSIXTime
    } deriving Show

PlutusTx.makeLift ''VestingParam

-- here we pass in the VestingParam as (p) before the datum which is now a () unit
-- where before in the where predicate we used a call to datum to get the beneficiary info and deadline info we now grab it from the parameterize VestingParam (p) object
{-# INLINABLE mkValidator #-}
mkValidator :: VestingParam -> () -> () -> ScriptContext -> Bool
mkValidator p () () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                          traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ beneficiary p

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline p) $ txInfoValidRange info

-- the types for the datum and redeemer are both () now since the VestingParam object contains all the necessary information for the transaction validation
data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = ()
    type instance RedeemerType Vesting = ()

-- to compile we need the VestingParam 
    -- first we need to lift out the code from p so Plutus can compile the mkValidator code from the code p has to generate
        -- on the haskell side there is mkValidator and p and we want <mkValidator p>
        -- on plutus core at compile time we have <mkValidator>  <p> <- uncompiled and want <mkValidator p> 
            -- we can do <mkValidator 'applyCode' <p> <- uncomiled
        -- we need to use lift code from p to compile it at run time then apply code to apply the lifted code to get what we want
            -- this way we can take plutus compile time compile <mkValidator> compile time uncompiled <p> and use liftCode on <p> to get it to be compiled on compile time to plutus core then use applyCode on the compiled mkValidator to compile it all together
        -- ensure you use get an instance of the VestingParam type with Lift type as seen above with makeLift using template Haskell
        -- we use multiParamTypeClasses to allow multiple typeclass instances 
typedValidator :: VestingParam -> Scripts.TypedValidator Vesting
typedValidator p = Scripts.mkTypedValidator @Vesting
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()

-- this now also needs a VestingParam and compose with typedValidator
validator :: VestingParam -> Validator
validator = Scripts.validatorScript . typedValidator

-- as does this to get the hash of the validator
valHash :: VestingParam -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedValidator

-- same with this to generate the script address
scrAddress :: VestingParam -> Ledger.Address
scrAddress = scriptAddress . validator

data GiveParams = GiveParams
    { gpBeneficiary :: !PaymentPubKeyHash
    , gpDeadline    :: !POSIXTime
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
            Endpoint "give" GiveParams
        .\/ Endpoint "grab" POSIXTime -- now need the deadline for grab

give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
    let p  = VestingParam
                { beneficiary = gpBeneficiary gp
                , deadline    = gpDeadline gp
                }
        tx = Constraints.mustPayToTheScript () $ Ada.lovelaceValueOf $ gpAmount gp
    ledgerTx <- submitTxConstraints (typedValidator p) tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

grab :: forall w s e. AsContractError e => POSIXTime -> Contract w s e ()
grab d = do
    now   <- currentTime
    pkh   <- ownPaymentPubKeyHash
    if now < d -- before deadline condition
    then logInfo @String $ "too early" -- log out before deadline 
    else do -- after deadline 
        let p = VestingParam -- make param == deadline passed in and pubKeyHash of caller
                    { beneficiary = pkh
                    , deadline    = d
                    }
        -- gather all utxos for address
        utxos <- utxosAt $ scrAddress p 
        -- check if null
        if Map.null utxos
        then logInfo @String $ "no gifts available" -- no txs for caller
        else do -- txs for caller
            let orefs   = fst <$> Map.toList utxos
                -- create lookup saying we want all unspent utxos with the validator matching the one we defined at p
                lookups = Constraints.unspentOutputs utxos      <>
                            Constraints.otherScript (validator p)
                -- create a a transaction with constraint saying we want to spend all refs using list comprehension. we also constrain it saying we only want the ones that will validate from now forward
                tx :: TxConstraints Void Void
                tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
                            Constraints.mustValidateIn (from now)
            -- submit the transaction with the constraints 
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            -- wait for confirmation
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            -- log the output
            logInfo @String $ "collected gifts"

endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []
