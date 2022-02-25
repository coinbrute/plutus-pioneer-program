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

module Week6.Oracle.Funds
    ( ownFunds
    , ownFunds'
    ) where

import           Control.Monad    hiding (fmap)
import qualified Data.Map         as Map
import           Data.Monoid      (Last (..))
import           Data.Text        (Text)
import           Plutus.Contract  as Contract
import           PlutusTx.Prelude hiding ((<$>))
import           Prelude          (Show (..), String, (<$>))
import           Ledger           hiding (singleton)
import           Ledger.Value     as Value

-- this is a helper module to get funds 

-- this retrieves the callers funds at the address 
ownFunds :: Contract w s Text Value
ownFunds = do
    -- retrieve the public key
    pk    <- ownPubKey
    -- get map of utxos at address from public key
    utxos <- utxoAt $ pubKeyAddress pk
    -- v is a sum of a list of the values of all the utxos that the address of the pk owns
    let v = mconcat $ Map.elems $ txOutValue . txOutTxOut <$> utxos
    -- log result
    logInfo @String $ "own funds: " ++ show (Value.flattenValue v)
    -- return v 
    return v

-- variation that doesn't return but rather tells it to state
ownFunds' :: Contract (Last Value) Empty Text ()
ownFunds' = do
    -- calls ownFunds then uses monadic bind to tell the value
    handleError logError $ ownFunds >>= tell . Last . Just
    -- we then wait 1 slot
    void $ Contract.waitNSlots 1
    -- then run again
    ownFunds'
    -- this will run forever and write to log
