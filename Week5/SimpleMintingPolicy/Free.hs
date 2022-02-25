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

module Week5.Free where

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

-- in this example minting policy will mint or burn no matter what and alway validate to true
    -- here we use redeemer type of Unit
{-# INLINABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy () _ = True

-- here we compile the policy to plutus script
    -- we do a very similar thing to validation scripts except we call wrapMintingPolicy on our mkPolicy function then wrap that in the compile call then call mkMintingPolicyScript on that
policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])

-- get the currency symbol with the built in function scriptCurrencySymbol
curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy

-- create our MintParams
    -- our mpTokenName is of type TokenName
    -- the mpAmount is an Integer
data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

-- define our off-chain endpoint type
type FreeSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
    -- compute the value by creating a singleton value by extracting the TokenName and amount
    let val     = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp)
    -- specify minting policy by passing in the policy
        lookups = Constraints.mintingPolicy policy
    -- get the tx constraint which is the value we need to mint
        tx      = Constraints.mustMintValue val
    -- submit the tx with the constraints and lookups
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    -- await confirmation
    void $ awaitTxConfirmed $ txId ledgerTx
    -- log message
    Contract.logInfo @String $ printf "forged %s" (show val)

-- define the endpoint function
endpoints :: Contract () FreeSchema Text ()
-- recursively call endpoints to mint prime where mint prime binds mint to the endpoint 
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''FreeSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    -- set token name
    let tn = "ABC"
    -- bind the wallets to the endpoint
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    -- call endpoint on wallet 1 with 555 as amount
    -- and wallet 2 with 444 as amount
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 555
        }
    callEndpoint @"mint" h2 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 444
        }
    -- wait 1 slot
    void $ Emulator.waitNSlots 1
    -- burn 222 from wallet 1
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = -222
        }
    -- wait another 1 slot
    void $ Emulator.waitNSlots 1
