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

module Week05.Signed where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
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
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

-- here we will have a minting policy where the transaction can only happen when the pkh matches and is validated
{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
-- so here we check that the scriptContextTxInfo in the ScriptContext is signed by the pkh passed in
mkPolicy pkh () ctx = txSignedBy (scriptContextTxInfo ctx) pkh

-- this is similar to before but we need to lift the code from the pkh and apply that lifted code to the mkPolicy before wraping and compiling it into a minting policy script
policy :: PubKeyHash -> Scripts.MintingPolicy
policy pkh = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    (PlutusTx.liftCode pkh)

-- this is similar to before except we evaluate everything on the right first since we are passing the pkh in
curSymbol :: PubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

-- this is the same as unsigned
data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

-- this is the same as unsigned though its renamed
type SignedSchema = Endpoint "mint" MintParams


mint :: MintParams -> Contract w SignedSchema Text ()
mint mp = do
    -- we get our own pkh by applying some functor mapping to the Contract.ownPubKey on pubKeyHash
    pkh <- pubKeyHash <$> Contract.ownPubKey
    -- now we use that pkh to create a singleton Value with those MintingParam tokenName and amount
    let val     = Value.singleton (curSymbol pkh) (mpTokenName mp) (mpAmount mp)
    -- set the lookup constraint with the policy passing in the pkh
        lookups = Constraints.mintingPolicy $ policy pkh
    -- set the must mint amount
        tx      = Constraints.mustMintValue val
    -- submit tx with constraints
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    -- await confirmation
    void $ awaitTxConfirmed $ txId ledgerTx
    -- log message
    Contract.logInfo @String $ printf "forged %s" (show val)

-- define endpoint function
endpoints :: Contract () SignedSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >>= mint

-- renamed schema
mkSchemaDefinitions ''SignedSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 555
        }
    callEndpoint @"mint" h2 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 444
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = -222
        }
    void $ Emulator.waitNSlots 1

-- NOTE that because these are parameterized minting policies each time they are minted to a wallet the currency symbol will be different even if the token name is the same. so in the test run above the output would be something like at the end when wallets were shown
{-
Wallet 1:
    {. ""}: 99983448
    {668b1c0bad3bc, "ABC"}: 333
Wallet 2:
    {. ""}: 99991724
    {bc43df2a5b74d, "ABC"}: 444
-}