{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week02.Gift where

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
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = () -- always passes regardless of inputs 
-- this basically would be a validator saying whatever ada is passed in is available for anyone to retrieve no matter whom or where they come from

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||]) -- uses template haskell to compile
-- [|| ||] these allow access to the underlying code stream within the mkValidator function for the compile function
-- $$ this syntax takes the syntax tree and splices into the source code at that point. 
-- PlutusTx.compile [|| mkValidator ||] gives us the plutus core compiled code
-- add $$() before compilation we get the spliced in code and turned into a validator

valHash :: Ledger.ValidatorHash -- hash of the validator
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address -- address for the validator script
scrAddress = scriptAddress validator
{-
Address {addressCredential = ScriptCredential 67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656, addressStakingCredential = Nothing}
-}

