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

module Week6.Oracle.Core
    ( Oracle (..)
    , OracleRedeemer (..)
    , oracleTokenName
    , oracleValue
    , oracleAsset
    , typedOracleValidator
    , oracleValidator
    , oracleAddress
    , OracleSchema
    , OracleParams (..)
    , runOracle
    , findOracle
    ) where

import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Plutus.Contracts.Currency as Currency
import           Prelude                   (Semigroup (..), Show (..), String)
import qualified Prelude

data Oracle = Oracle
    { oSymbol   :: !CurrencySymbol -- the currency symbol of the NFT
    , oOperator :: !PubKeyHash -- the owner of the oracle who can make updates and collect fees
    , oFee      :: !Integer -- fee amount in lovelace to collect each tx
    , oAsset    :: !AssetClass -- identifies the target of the oracle i.e. ada to x this is x
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Oracle

-- data type to allow for the update and use operations
data OracleRedeemer = Update | Use
    deriving Show

-- template haskell for is data on redeemer
PlutusTx.unstableMakeIsData ''OracleRedeemer

-- oracleToken name
{-# INLINABLE oracleTokenName #-}
oracleTokenName :: TokenName
oracleTokenName = TokenName emptyByteString

-- pass in an Oracle and return an AssetClass
    -- this is the NFT of the Oracle Value
{-# INLINABLE oracleAsset #-}
oracleAsset :: Oracle -> AssetClass
oracleAsset oracle = AssetClass (oSymbol oracle, oracleTokenName)

-- given a TxOut and a DatumHash we can get the datum and turn it into an integer
{-# INLINABLE oracleValue #-}
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer
oracleValue o f = do
    -- try to get the txOut datum
    dh      <- txOutDatum o
    -- the turn the datum hash into a datum
    Datum d <- f dh
    -- if we have a datum then turn it into an integer
    PlutusTx.fromBuiltinData d

-- we take the oracle and an integer along with a redeemer and context and returns whether is valid
{-# INLINABLE mkOracleValidator #-}
mkOracleValidator :: Oracle -> Integer -> OracleRedeemer -> ScriptContext -> Bool
mkOracleValidator oracle x r ctx =
    -- check if nft is in input and output
    traceIfFalse "token missing from input"  inputHasToken  &&
    traceIfFalse "token missing from output" outputHasToken &&
    -- go the update or use operation cases
    case r of
        -- check that operator signed the transaction
        Update -> traceIfFalse "operator signature missing" (txSignedBy info $ oOperator oracle) &&
        -- and that output datum is valid
                  traceIfFalse "invalid output datum"       validOutputDatum
        -- check that the outputDatum == x and that fees are paid
        Use    -> traceIfFalse "oracle value changed"       (outputDatum == Just x)              &&
                  traceIfFalse "fees not paid"              feesPaid
  where
    -- get the tx from the context
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- this is the output that we are trying to consume
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i

    -- assetClassValueOf checks how many are at the nft which should be there 1
    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oracleAsset oracle) == 1

    -- now we check the context for outputs on the script address currently being validated
    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle output"

    -- we do the same check as above but call ownOutput
    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleAsset oracle) == 1

    -- check the ownOutput on the output from findDatum on info
    outputDatum :: Maybe Integer
    outputDatum = oracleValue ownOutput (`findDatum` info)

    -- check the outputDatum is a just Integer
    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatum

    -- ensure the fees are paid 
    feesPaid :: Bool
    feesPaid =
      let
        -- get the out value from input and output
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
      in
        -- out value should be greater than or equal to in value and lovlaceValue of oFee of Oracle
        outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))

-- create instance of ValidatorType of Oracling
    -- this combines the datum and redeemer
data Oracling
instance Scripts.ValidatorTypes Oracling where
    type instance DatumType Oracling = Integer
    type instance RedeemerType Oracling = OracleRedeemer

-- this combines the oracle to a plutus validator
    -- we have to lift the paramterized code and then compile 
typedOracleValidator :: Oracle -> Scripts.TypedValidator Oracling
typedOracleValidator oracle = Scripts.mkTypedValidator @Oracling
    ($$(PlutusTx.compile [|| mkOracleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @OracleRedeemer

-- we can turn it into a validator now
oracleValidator :: Oracle -> Validator
oracleValidator = Scripts.validatorScript . typedOracleValidator

-- and now get the address for the validator
oracleAddress :: Oracle -> Ledger.Address
oracleAddress = scriptAddress . oracleValidator

-- here we create a data type for typed params for offchain use
data OracleParams = OracleParams
    { opFees   :: !Integer
    , opSymbol :: !CurrencySymbol -- this is the currency symbol of what we want to check
    , opToken  :: !TokenName -- this is the token we want to check
    } deriving (Show, Generic, FromJSON, ToJSON)

-- here we make a contract for the start of the oracle
    -- all this does is mint the nft for the oracle 
startOracle :: forall w s. OracleParams -> Contract w s Text Oracle
startOracle op = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    -- we use the mintContract here to mint the nft for the oracle
        -- it just takes a PubKeyHash and a list of pairs of TokenNames and Integers
    -- most of this line is converting the Text error type of the main function to CurrencyError in the mintContract call here
    osc <- mapError (pack . show) (mintContract pkh [(oracleTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency)
    let cs     = Currency.currencySymbol osc
        oracle = Oracle
            { oSymbol   = cs
            , oOperator = pkh -- we use ourself which we lookup above
            , oFee      = opFees op
            , oAsset    = AssetClass (opSymbol op, opToken op)
            }
    -- log a message
    logInfo @String $ "started oracle " ++ show oracle
    -- return out the oracle
    return oracle


-- here is the contract for the update oracle
    -- here we deal with two cases one where we have a new oracle where we need to set initial value and one where we need to find and update the value
updateOracle :: forall w s. Oracle -> Integer -> Contract w s Text ()
updateOracle oracle x = do
    -- find the oracle utxo
    m <- findOracle oracle
    -- we must pay the amount for fee to script
    -- we must have some assetClassValue to attache to the oracle i.e. the nft
    let c = Constraints.mustPayToTheScript x $ assetClassValue (oracleAsset oracle) 1
    case m of
        -- we didn't find the oracle
        Nothing -> do
            -- create tx to produce first oracle output 
                -- we only need constraint c
            ledgerTx <- submitTxConstraints (typedOracleValidator oracle) c
            -- await confirmation
            awaitTxConfirmed $ txId ledgerTx
            -- log message
            logInfo @String $ "set initial oracle value to " ++ show x
        -- we have the utxo
            -- we only care about the oref and the o we are updating the last value
        Just (oref, o,  _) -> do
            -- unspentOuputs gives a map of utxos we want to consume
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>
                        -- these provide the script instances once for the input side and once for the output side
                          Constraints.typedValidatorLookups (typedOracleValidator oracle) <>
                          Constraints.otherScript (oracleValidator oracle)
            -- then we tack addition constraints onto our primary ones saying we must spend the scrpt output from the oref 
                -- this says we consume it and create a new input 
                -- this takes a redeemer which is Update in this case
                tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Update)
            -- submit the transaction with constraints
            -- the balancing algorithm will adjust inputs for fees and create extra utxos to account for fee modeling as necessary
            ledgerTx <- submitTxConstraintsWith @Oracling lookups tx
            -- await confirmation
            awaitTxConfirmed $ txId ledgerTx
            -- log message
            logInfo @String $ "updated oracle value to " ++ show x

-- this is a helper function to find an oracle
    -- it takes an oracle and looks up the oracle in utxos and returns a Just with a triple or a Nothing
findOracle :: forall w s. Oracle -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Integer))
findOracle oracle = do
    -- get all utxos sitting at oracle address
        -- filter out any occurance where the nft is there 0 or > 1
    utxos <- Map.filter f <$> utxoAt (oracleAddress oracle)
    return $ case Map.toList utxos of
        [(oref, o)] -> do
            -- bind to x grabbing the datum hash and converting it to an integer value then returning as a triple or Nothing
            x <- oracleValue (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o
            return (oref, o, x)
        _           -> Nothing
  where
    -- check the value of the utxo and check that the nft is only there once
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) (oracleAsset oracle) == 1

-- define schema endpoint
type OracleSchema = Endpoint "update" Integer

-- this is for use in the emulator or playground
runOracle :: OracleParams -> Contract (Last Oracle) OracleSchema Text ()
runOracle op = do
    -- we startOracle on the params passed in
    oracle <- startOracle op
    -- use tell to write the last Just value from the oracle
    tell $ Last $ Just oracle
    -- call go on oracle
    go oracle
  where
    -- takes an oracle and returns a contract of oracle
    go :: Oracle -> Contract (Last Oracle) OracleSchema Text a
    go oracle = do
        -- waits at endpoint until integer is given then binds
        x <- endpoint @"update"
        -- updates oracle with x
        updateOracle oracle x
        -- calls again with oracle
        go oracle
