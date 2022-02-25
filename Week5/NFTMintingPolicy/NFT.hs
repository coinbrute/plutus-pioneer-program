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

module Week5.NFT where

import           Control.Monad          hiding (fmap)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

-- this minting policy will only allow minting of a certain amount and that the utxo containing the datum with the value to be minted has not been consumed already
    -- we check a specific outgoing utxo ref and a token name 
    -- this way since utxos are unique if a utxo has already been consumed and is being passed in we know it cannot be validated against in the policy
{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                          traceIfFalse "wrong amount minted" checkMintedAmount
  where
    -- here we get the TxInfo from the ScriptContext passed in by calling scriptContextTxInfo on it
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- first we check if there are any inputs by getting a list of inputs from info
        -- then we get any outRefs from them that are equal to the oref passed in 
        -- return the bool of that function
    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    -- first we flatten the value of txInfoForge from info
        -- then we run pattern matching on it 
            -- there should only be one match so any other result return false 
                -- else if the currencySymbol  == currency symbol in the script context and the amount is one
            -- but how do we know the (cs) in the triple is the correct one?
                -- it is done with the ownCurrencySymbol by calling it on the Context. 
                -- this ensures we use the one from the compiler and not from the runtime usage
                -- this is overkill here as we are checking that we only forge 1 of them so there can never be more than 1
    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoForge info) of
        [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == tn && amt == 1
        _                -> False

-- here we need to lift out both the oref and the tn then apply them before wrapping and compiling as normal
policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tn = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn

-- compute currency symbol by adding the new parameters
curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn

-- off chain endpoint type which takes a TokenName
type NFTSchema = Endpoint "mint" TokenName

-- this takes the TokenName and mints the contract
mint :: TokenName -> Contract w NFTSchema Text ()
mint tn = do
    -- look up our own pubKey 
    pk    <- Contract.ownPubKey
    -- find a suitable utxo to pass to the policy to be consumed 
        -- we pass our address to utxoAt to retrieve a list of utxos for our address
    utxos <- utxoAt (pubKeyAddress pk)
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        -- here we grab the first utxo and use it for the 
        oref : _ -> do
            -- create a singleton Value using the Currency symbol from the utxo and tokenName, then the token name, and amount of 1
            let val     = Value.singleton (curSymbol oref tn) tn 1
            -- we need the minting policy passing the policy above with the oref we grabbed and the tokenName
            -- we also need the unspent outputs i.e. the utxos 
                lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
            -- we also need to provide the mustMintValue using the value
            -- we also need to provide the output to consume i.e. oref
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            -- submit the tx with the constraints
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            -- await confirmations
            void $ awaitTxConfirmed $ txId ledgerTx
            -- log message
            Contract.logInfo @String $ printf "forged %s" (show val)

-- define endpoints
endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''NFTSchema

mkKnownCurrencies []

-- emulator trace test
test :: IO ()
test = runEmulatorTraceIO $ do
    -- define token name
    let tn = "ABC"
    -- activate contract wallets on endpoint
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    -- call mint endpoint on both wallets
    callEndpoint @"mint" h1 tn
    callEndpoint @"mint" h2 tn
    -- wait 1 slot
    void $ Emulator.waitNSlots 1
