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

module Week8.TokenSale
    ( TokenSale (..)
    , TSRedeemer (..)
    , TSStartSchema
    , TSUseSchema
    , startEndpoint
    , useEndpoints
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value
import           Prelude                      (Semigroup (..), Show (..), uncurry)
import qualified Prelude

-- here we implement the token sale example

data TokenSale = TokenSale
    { tsSeller :: !PubKeyHash -- pub key hash of seller
    , tsToken  :: !AssetClass -- token being stored and sold
    , tsTT     :: !(Maybe ThreadToken) -- way of tracking the utxo 
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''TokenSale

-- methods for the seller for use as a redeemer 
data TSRedeemer =
      SetPrice Integer
    | AddTokens Integer
    | BuyTokens Integer
    | Withdraw Integer Integer -- first is tokens second is lovelace
    deriving (Show, Prelude.Eq)

PlutusTx.unstableMakeIsData ''TSRedeemer

-- extracts lovelace from value
{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

-- get the state and perform associated operation per the predicate value and return the 
{-# INLINABLE transition #-}
transition :: TokenSale -> State Integer -> TSRedeemer -> Maybe (TxConstraints Void Void, State Integer)
-- pattern match on the triple
transition ts s r = case (stateValue s, stateData s, r) of
    -- go through and check each operation and condition

    -- set price must be signed by the seller
      -- new price will the the price p
    (v, _, SetPrice p)   | p >= 0           -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                    , State p v
                                                    )
    -- add tokens say that new amount (n) must be positive 
      -- update the state    
      -- we don't require the seller to sign as we allow donations                                             
    (v, p, AddTokens n)  | n > 0            -> Just ( mempty
                                                    , State p $
                                                      v                                       <>
                                                      assetClassValue (tsToken ts) n
                                                    )
    -- anybody can buy tokens 
      -- we update the stae but update the number of tokens                                                
    (v, p, BuyTokens n)  | n > 0            -> Just ( mempty
                                                    , State p $
                                                      v                                       <>
                                                      assetClassValue (tsToken ts) (negate n) <>
                                                      lovelaceValueOf (n * p)
                                                    )
    -- here we withdraw and require the seller to sign the transaction
      -- we say that both amounts must be at least 0                                                    
    (v, p, Withdraw n l) | n >= 0 && l >= 0 -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                    , State p $
                                                      v                                       <>
                                                      assetClassValue (tsToken ts) (negate n) <>
                                                      lovelaceValueOf (negate l)
                                                    )
    -- all other cases return nothing                                                    
    _                                       -> Nothing

-- define our state machine
  -- we use mkStateMachine which takes the thread token the transition function and a decider of whether there is a final state which we don't have
  -- we can use this since we defined our transitions using all Contraints
{-# INLINABLE tsStateMachine #-}
tsStateMachine :: TokenSale -> StateMachine Integer TSRedeemer
tsStateMachine ts = mkStateMachine (tsTT ts) (transition ts) (const False)

-- make the state machine into a validator
{-# INLINABLE mkTSValidator #-}
mkTSValidator :: TokenSale -> Integer -> TSRedeemer -> ScriptContext -> Bool
mkTSValidator = mkValidator . tsStateMachine

type TS = StateMachine Integer TSRedeemer

-- compile into a typed validator
tsTypedValidator :: TokenSale -> Scripts.TypedValidator TS
tsTypedValidator ts = Scripts.mkTypedValidator @TS
    ($$(PlutusTx.compile [|| mkTSValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode ts)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @TSRedeemer

-- get the validator
tsValidator :: TokenSale -> Validator
tsValidator = Scripts.validatorScript . tsTypedValidator

-- get the address
tsAddress :: TokenSale -> Ledger.Address
tsAddress = scriptAddress . tsValidator

-- define the client for the sm used to interact with the sm from off chain code
tsClient :: TokenSale -> StateMachineClient Integer TSRedeemer
tsClient ts = mkStateMachineClient $ StateMachineInstance (tsStateMachine ts) (tsTypedValidator ts)

-- pack and show the contract error into Text
mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
mapErrorSM = mapError $ pack . show

-- start the and run the token sale state machine
  -- takes the assetclass i.e. token
  -- boolean to rep the thread token usage true means use false means dont

startTS :: AssetClass -> Bool -> Contract (Last TokenSale) s Text ()
startTS token useTT = do
    -- get sellers public key hash
    pkh <- pubKeyHash <$> Contract.ownPubKey
    -- get the thread key if bool is true
    tt  <- if useTT then Just <$> mapErrorSM getThreadToken else return Nothing
    -- define the TokenSale
    let ts = TokenSale
            { tsSeller = pkh
            , tsToken  = token
            , tsTT     = tt
            }
        -- set the client for the state machine
        client = tsClient ts
    -- create first utxo at state machine address
      -- runInitialise takes client datum and value with value being the 0 mempty
    void $ mapErrorSM $ runInitialise client 0 mempty
    -- tell the last ts
      -- this is for outside parties to find the utxo
    tell $ Last $ Just ts
    -- log message
    logInfo $ "started token sale " ++ show ts

-- take token sale and price amount
  -- runStep to cause transition in sm passing client and the SetPrice of p
setPrice :: TokenSale -> Integer -> Contract w s Text ()
setPrice ts p = void $ mapErrorSM $ runStep (tsClient ts) $ SetPrice p

-- similar to setPrice
addTokens :: TokenSale -> Integer -> Contract w s Text ()
addTokens ts n = void $ mapErrorSM $ runStep (tsClient ts) $ AddTokens n

-- similar to setPrice
buyTokens :: TokenSale -> Integer -> Contract w s Text ()
buyTokens ts n = void $ mapErrorSM $ runStep (tsClient ts) $ BuyTokens n

-- similar to setPrice but with extra integer
withdraw :: TokenSale -> Integer -> Integer -> Contract w s Text ()
withdraw ts n l = void $ mapErrorSM $ runStep (tsClient ts) $ Withdraw n l

-- define StartSchema
  -- this takes a triple
    -- contains a currency symbol and tokenname i.e. asset class
    -- bool
type TSStartSchema =
        Endpoint "start"      (CurrencySymbol, TokenName, Bool)
-- define use schema
type TSUseSchema =
        Endpoint "set price"  Integer
    .\/ Endpoint "add tokens" Integer
    .\/ Endpoint "buy tokens" Integer
    .\/ Endpoint "withdraw"   (Integer, Integer)

-- define versions for start endpoint
startEndpoint :: Contract (Last TokenSale) TSStartSchema Text ()
startEndpoint = forever -- forever repeat
              -- handle any errors
              $ handleError logError
              -- we turn it into a contract with awaitPromise
              $ awaitPromise
              -- specify name of endpoint then the parameters we will be using in the promisary return that blocks and waits for user input. 
                -- that user input is bound to (cs, tn, useTT)
              $ endpoint @"start" $ \(cs, tn, useTT) -> startTS (AssetClass (cs, tn)) useTT

-- define versions for use endpoints
useEndpoints :: TokenSale -> Contract () TSUseSchema Text ()
-- first three lines are the same as above
useEndpoints ts = forever
                $ handleError logError
                $ awaitPromise
                -- select allows for a continuous selection between the optins
                $ setPrice' `select` addTokens' `select` buyTokens' `select` withdraw'
  where
    -- here are the promises like above
    setPrice'  = endpoint @"set price"  $ setPrice ts
    addTokens' = endpoint @"add tokens" $ addTokens ts
    buyTokens' = endpoint @"buy tokens" $ buyTokens ts
    withdraw'  = endpoint @"withdraw"   $ uncurry (withdraw ts)
