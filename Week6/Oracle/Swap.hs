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

module Week6.Oracle.Swap
    ( SwapSchema
    , swap
    ) where

import           Control.Monad        hiding (fmap)
import           Data.List            (find)
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import           Data.Monoid          (Last (..))
import           Data.Text            (Text)
import           Plutus.Contract      as Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), (<$>), unless, mapMaybe, find)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada hiding (divide)
import           Ledger.Value         as Value
import           Prelude              (Semigroup (..), Show (..), String, (<$>))

import           Week6.Oracle.Core
import           Week6.Oracle.Funds

-- the idea here is someone can put in ada and someone else can exchange that ada for usdt

-- the price will be determined by the oracle
{-# INLINABLE price #-}
price :: Integer -> Integer -> Integer
price lovelace exchangeRate = (lovelace * exchangeRate) `divide` 1000000

-- given a value extract the amount of lovelace
{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

-- this validator takes an Oracle an address for the oracle a pkh for the datum which is the seller and unit for the redeemer
{-# INLINABLE mkSwapValidator #-}
mkSwapValidator :: Oracle -> Address -> PubKeyHash -> () -> ScriptContext -> Bool
mkSwapValidator oracle addr pkh () ctx =
    -- check that tx is signed by seller and TxInfo
    txSignedBy info pkh ||
    -- OR
    -- check that exactly two script inputs exist the oracle and the script utxo 
    (traceIfFalse "expected exactly two script inputs" hasTwoScriptInputs &&
    -- also check that the seller got paid
     traceIfFalse "price not paid"                     sellerPaid)

  where
    -- get the tx info
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- get the oracleInput
    oracleInput :: TxOut
    oracleInput =
      let
        -- o must == the address for the oracle and we keep those 
          -- then we want to only have 1 of those at the end of list comprehension
        ins = [ o | i <- txInfoInputs info, let o = txInInfoResolved i, txOutAddress o == addr]
      in
        -- check ins
        case ins of
            [o] -> o
            _   -> traceError "expected exactly one oracle input"

    -- check exchange rate by converting datum hash to Integer value and return x
    oracleValue' = case oracleValue oracleInput (`findDatum` info) of
        Nothing -> traceError "oracle value not found"
        Just x  -> x

    -- check for the two script inputs
    hasTwoScriptInputs :: Bool
    hasTwoScriptInputs =
      let -- loop over all inputs and filter 
        -- get the inputs from info then see what are resolved and are on the send address and part of the validator hash 
        xs = filter (isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
      in
        length xs == 2

    -- compute the min price for seller to be paid
    minPrice :: Integer
    minPrice =
      let
        -- find own input for the script context
        lovelaceIn = case findOwnInput ctx of
            Nothing -> traceError "own input not found"
            -- how many lovelaces are here at this swapped utxo
            Just i  -> lovelaces $ txOutValue $ txInInfoResolved i
      in
        -- given this amount and the oracle value give us the price
        price lovelaceIn oracleValue'

    -- check that the seller got paid
    sellerPaid :: Bool
    sellerPaid =
      let
        -- based on how much was paid given info and pkh add up the total paid from here to seller
        pricePaid :: Integer
        pricePaid =  assetClassValueOf (valuePaidTo info pkh) (oAsset oracle)
      in
        -- is it enough
        pricePaid >= minPrice

-- define the data type for Swapping making an instance of validatorTypes
data Swapping
instance Scripts.ValidatorTypes Swapping where
    type instance DatumType Swapping = PubKeyHash
    type instance RedeemerType Swapping = ()

-- template haskell stuff for compiling to Plutus core
typedSwapValidator :: Oracle -> Scripts.TypedValidator Swapping
typedSwapValidator oracle = Scripts.mkTypedValidator @Swapping
    ($$(PlutusTx.compile [|| mkSwapValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode oracle
        -- here we actually just lift the oracle instead of both params
        `PlutusTx.applyCode` PlutusTx.liftCode (oracleAddress oracle))
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PubKeyHash @()

-- get the validator
swapValidator :: Oracle -> Validator
swapValidator = Scripts.validatorScript . typedSwapValidator

-- get the address
swapAddress :: Oracle -> Ledger.Address
swapAddress = scriptAddress . swapValidator

-- this is for the seller to provide a swap
  -- integer is amount of lovelace to offer
offerSwap :: forall w s. Oracle -> Integer -> Contract w s Text ()
offerSwap oracle amt = do
    -- look up sellers pkh
    pkh <- pubKeyHash <$> Contract.ownPubKey
    -- constraint is must pay to the script since as a seller we are paying into the script
    let tx = Constraints.mustPayToTheScript pkh $ Ada.lovelaceValueOf amt
    -- submit tx with constraint
    ledgerTx <- submitTxConstraints (typedSwapValidator oracle) tx
    -- await confirmation
    awaitTxConfirmed $ txId ledgerTx
    -- log message
    logInfo @String $ "offered " ++ show amt ++ " lovelace for swap"

-- this will find a list swaps that sit at swap address 
  -- the pkh is the datum for the utxo 
findSwaps :: Oracle -> (PubKeyHash -> Bool) -> Contract w s Text [(TxOutRef, TxOutTx, PubKeyHash)]
findSwaps oracle p = do
    -- this returns a list of all utxos for oracle address 
    utxos <- utxoAt $ swapAddress oracle
    -- this applies mapMaybe to the utxos list to filter them
    return $ mapMaybe g $ Map.toList utxos
  where
    -- this function will return a Maybe PubKeyHash on the TxOutTx given
    f :: TxOutTx -> Maybe PubKeyHash
    f o = do
        -- bind a datum hash from a txOutTxOut passed in
        dh        <- txOutDatumHash $ txOutTxOut o
        -- attempt to get the datum from the hash
        (Datum d) <- Map.lookup dh $ txData $ txOutTxTx o
        -- deserialize the pkh from the datum
        PlutusTx.fromBuiltinData d

    -- function for mapMaybe
    g :: (TxOutRef, TxOutTx) -> Maybe (TxOutRef, TxOutTx, PubKeyHash)
    g (oref, o) = do
        -- get the pkh
        pkh <- f o
        -- check if predicate of guard is satisfied from pkh then return triple
        guard $ p pkh
        return (oref, o, pkh)

-- for seller and wants to swap back
retrieveSwaps :: Oracle -> Contract w s Text ()
retrieveSwaps oracle = do
    -- gets the sellers pkh
    pkh <- pubKeyHash <$> ownPubKey
    -- find swaps for pkh matches
    xs  <- findSwaps oracle (== pkh)
    case xs of
        [] -> logInfo @String "no swaps found"
        _  -> do
            -- retrieve all unspent utxos as a list using the oref and the o 
            let lookups = Constraints.unspentOutputs (Map.fromList [(oref, o) | (oref, o, _) <- xs]) <>
            -- we also need to provide the validator of the swapValidator parameterized by the oracle
                          Constraints.otherScript (swapValidator oracle)
            -- say we must spend them script outputs and then concat them all for all utxos using mconcat
                tx      = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData () | (oref, _, _) <- xs]
            -- submit tx
            ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
            -- await confirmation
            awaitTxConfirmed $ txId ledgerTx
            -- log message
            logInfo @String $ "retrieved " ++ show (length xs) ++ " swap(s)"

-- this is the meat and potatoes where a swap occurs and things are happenin'
useSwap :: forall w s. Oracle -> Contract w s Text ()
useSwap oracle = do
    funds <- ownFunds
    let amt = assetClassValueOf funds $ oAsset oracle
    logInfo @String $ "available assets: " ++ show amt

    m <- findOracle oracle
    case m of
        Nothing           -> logInfo @String "oracle not found"
        Just (oref, o, x) -> do
            logInfo @String $ "found oracle, exchange rate " ++ show x
            pkh   <- pubKeyHash <$> Contract.ownPubKey
            swaps <- findSwaps oracle (/= pkh)
            case find (f amt x) swaps of
                Nothing                -> logInfo @String "no suitable swap found"
                Just (oref', o', pkh') -> do
                    let v       = txOutValue (txOutTxOut o) <> lovelaceValueOf (oFee oracle)
                        p       = assetClassValue (oAsset oracle) $ price (lovelaces $ txOutValue $ txOutTxOut o') x
                        lookups = Constraints.otherScript (swapValidator oracle)                     <>
                                  Constraints.otherScript (oracleValidator oracle)                   <>
                                  Constraints.unspentOutputs (Map.fromList [(oref, o), (oref', o')])
                        tx      = Constraints.mustSpendScriptOutput oref  (Redeemer $ PlutusTx.toBuiltinData Use) <>
                                  Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData ())  <>
                                  Constraints.mustPayToOtherScript
                                    (validatorHash $ oracleValidator oracle)
                                    (Datum $ PlutusTx.toBuiltinData x)
                                    v                                                                             <>
                                  Constraints.mustPayToPubKey pkh' p
                    ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
                    awaitTxConfirmed $ txId ledgerTx
                    logInfo @String $ "made swap with price " ++ show (Value.flattenValue p)
  where
    getPrice :: Integer -> TxOutTx -> Integer
    getPrice x o = price (lovelaces $ txOutValue $ txOutTxOut o) x

    f :: Integer -> Integer -> (TxOutRef, TxOutTx, PubKeyHash) -> Bool
    f amt x (_, o, _) = getPrice x o <= amt

type SwapSchema =
            Endpoint "offer"    Integer
        .\/ Endpoint "retrieve" ()
        .\/ Endpoint "use"      ()
        .\/ Endpoint "funds"    ()

swap :: Oracle -> Contract (Last Value) SwapSchema Text ()
swap oracle = (offer `select` retrieve `select` use `select` funds) >> swap oracle
  where
    offer :: Contract (Last Value) SwapSchema Text ()
    offer = h $ do
        amt <- endpoint @"offer"
        offerSwap oracle amt

    retrieve :: Contract (Last Value) SwapSchema Text ()
    retrieve = h $ do
        endpoint @"retrieve"
        retrieveSwaps oracle

    use :: Contract (Last Value) SwapSchema Text ()
    use = h $ do
        endpoint @"use"
        useSwap oracle

    funds :: Contract (Last Value) SwapSchema Text ()
    funds = h $ do
        endpoint @"funds"
        v <- ownFunds
        tell $ Last $ Just v

    h :: Contract (Last Value) SwapSchema Text () -> Contract (Last Value) SwapSchema Text ()
    h = handleError logError
