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

module Week7.StateMachine
    ( Game (..)
    , GameChoice (..)
    , FirstParams (..)
    , SecondParams (..)
    , GameSchema
    , Last (..)
    , ThreadToken
    , Text
    , endpoints
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import           Ledger.Typed.Tx
import qualified Ledger.Typed.Scripts         as Scripts
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Playground.Contract          (ToSchema)
import           Prelude                      (Semigroup (..), Show (..), String)
import qualified Prelude

-- this is an example of the even odd game but utilizing the StateMachine data type and concept

-- the game type is the same as EvenOdd
data Game = Game
    { gFirst          :: !PubKeyHash
    , gSecond         :: !PubKeyHash
    , gStake          :: !Integer
    , gPlayDeadline   :: !POSIXTime
    , gRevealDeadline :: !POSIXTime
    , gToken          :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''Game

-- GameChoice is the same as EvenOdd
data GameChoice = Zero | One
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

-- GameChoice instance of Eq is the same as EvenOdd
instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    Zero == Zero = True
    One  == One  = True
    _    == _    = False

PlutusTx.unstableMakeIsData ''GameChoice

-- game datum now has an additional constructor Finished for the finsihed state of the State Machine
data GameDatum = GameDatum ByteString (Maybe GameChoice) | Finished
    deriving Show

-- we expand the Eq instance of GameDatum to account for the Finished type
instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')
    Finished        == Finished          = True
    _               == _                 = False

PlutusTx.unstableMakeIsData ''GameDatum

-- the redeemer remains the same
data GameRedeemer = Play GameChoice | Reveal ByteString | ClaimFirst | ClaimSecond
    deriving Show

PlutusTx.unstableMakeIsData ''GameRedeemer

-- this remains the same
{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

-- this remains the same
{-# INLINABLE gameDatum #-}
gameDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe GameDatum
gameDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

-- here instead of a validator script we create a transition script which takes the following
    -- we recieve a Game
    -- a State in the form of a GameDatum 
    -- a GameRedeemer 
    -- then we return a maybe of constraints and a State of GameDatum
{-# INLINABLE transition #-}
transition :: Game -> State GameDatum -> GameRedeemer -> Maybe (TxConstraints Void Void, State GameDatum)
transition game s r = case (stateValue s, stateData s, r) of -- stateValue is the value we are consuming 
    -- we don't need to check that the input has the token since the state carries the token
    (v, GameDatum bs Nothing, Play c) -- player 1 moved player 2 moving
        -- check the value is the stake amount in the game
            -- the State check is the check for the output datum and the stake for player two being there
            -- the first two constraints are for the tx being signed by player 2 and deadline not being passed
        | lovelaces v == gStake game         -> Just ( Constraints.mustBeSignedBy (gSecond game)                    <>
                                                       Constraints.mustValidateIn (to $ gPlayDeadline game)
                                                     , State (GameDatum bs $ Just c) (lovelaceValueOf $ 2 * gStake game)
                                                     )
    (v, GameDatum _ (Just _), Reveal _)
        -- we check that the staking amount is correct
        -- we do not check the nonce in thsi version 
            -- we do check the tx is signed and the deadline is within range
        -- finally we send the nft back by sending th state to Finished
        | lovelaces v == (2 * gStake game)   -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <>
                                                       Constraints.mustValidateIn (to $ gRevealDeadline game)
                                                     , State Finished mempty
                                                     )
    (v, GameDatum _ Nothing, ClaimFirst)
        -- value must equal game stake
        -- tx must be signed and deadline must be passed
        -- state machine takes care of nft so we indicate state machine is finished
        | lovelaces v == gStake game         -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <>
                                                       Constraints.mustValidateIn (from $ 1 + gPlayDeadline game)
                                                     , State Finished mempty
                                                     )
    (v, GameDatum _ (Just _), ClaimSecond)
        -- ensure both stakes are in game 
        -- tx must be signed by player 2
        -- reveal deadline must be passed
        -- nft is handled by state machine we just transition to Finished state
        | lovelaces v == (2 * gStake game)   -> Just ( Constraints.mustBeSignedBy (gSecond game)                    <>
                                                       Constraints.mustValidateIn (from $ 1 + gRevealDeadline game)
                                                     , State Finished mempty
                                                     )
    -- all other things are invalid                                                 
    _                                        -> Nothing

-- specify the final state of the state machine
{-# INLINABLE final #-}
final :: GameDatum -> Bool
final Finished = True
final _        = False

-- this does the nonce check
    -- we only care about the nonce value inputted by the player
    -- we hash that and compare with the originally provided hash
{-# INLINABLE check #-}
check :: ByteString -> ByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
check bsZero' bsOne' (GameDatum bs (Just c)) (Reveal nonce) _ =
    sha2_256 (nonce `concatenate` if c == Zero then bsZero' else bsOne') == bs
check _       _      _                       _              _ = True

-- define the state machine
    -- provide the four fields we just defined 
        -- transition
        -- final
        -- check
        -- threadToken which we take from game token
{-# INLINABLE gameStateMachine #-}
gameStateMachine :: Game -> ByteString -> ByteString -> StateMachine GameDatum GameRedeemer
gameStateMachine game bsZero' bsOne' = StateMachine
    { smTransition  = transition game
    , smFinal       = final
    , smCheck       = check bsZero' bsOne'
    , smThreadToken = Just $ gToken game
    }

-- make the game validator 
{-# INLINABLE mkGameValidator #-}
mkGameValidator :: Game -> ByteString -> ByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsZero' bsOne' = mkValidator $ gameStateMachine game bsZero' bsOne'

-- bundle the types into a type of Gaming
type Gaming = StateMachine GameDatum GameRedeemer

-- bytestring conversion for inputs
bsZero, bsOne :: ByteString
bsZero = "0"
bsOne  = "1"

-- gameStateMachine prime using a Game returns a state machine with a datum and redeemer
gameStateMachine' :: Game -> StateMachine GameDatum GameRedeemer
gameStateMachine' game = gameStateMachine game bsZero bsOne

-- compilation function that lifts and applies the game bsZero and bsOne before compiling the mkGameVlaidator into a typed validator script Gaming
typedGameValidator :: Game -> Scripts.TypedValidator Gaming
typedGameValidator game = Scripts.mkTypedValidator @Gaming
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game
        `PlutusTx.applyCode` PlutusTx.liftCode bsZero
        `PlutusTx.applyCode` PlutusTx.liftCode bsOne)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer

-- get the validator
gameValidator :: Game -> Validator
gameValidator = Scripts.validatorScript . typedGameValidator

-- get the address
gameAddress :: Game -> Ledger.Address
gameAddress = scriptAddress . gameValidator

-- this is what we need to interact with the client from our wallet
    -- the StateMachineInstance is a constructor used to return a client which is a client side definition of the stateMachine
    -- we call mkStateMachineClient on the StateMachineInstance we construct
-- the cient can be used to interact with the state machine from off chain code
gameClient :: Game -> StateMachineClient GameDatum GameRedeemer
gameClient game = mkStateMachineClient $ StateMachineInstance (gameStateMachine' game) (typedGameValidator game)

data FirstParams = FirstParams
    { fpSecond         :: !PubKeyHash
    , fpStake          :: !Integer
    , fpPlayDeadline   :: !POSIXTime
    , fpRevealDeadline :: !POSIXTime
    , fpNonce          :: !ByteString
    , fpChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- we show the SMContractError then pack the resulting string into a Text
mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . show

-- wait until a time is reached then wait 1 slot
waitUntilTimeHasPassed :: AsContractError e => POSIXTime -> Contract w s e ()
waitUntilTimeHasPassed t = void $ awaitTime t >> waitNSlots 1

-- first game
firstGame :: forall s. FirstParams -> Contract (Last ThreadToken) s Text ()
firstGame fp = do
    -- get our public key
    pkh <- pubKeyHash <$> Contract.ownPubKey
    -- get a threadToken 
        -- we need to identify a utxo to be used for the nft
    tt  <- mapError' getThreadToken
    -- define the Game
    let game   = Game
            { gFirst          = pkh
            , gSecond         = fpSecond fp
            , gStake          = fpStake fp
            , gPlayDeadline   = fpPlayDeadline fp
            , gRevealDeadline = fpRevealDeadline fp
            , gToken          = tt
            }
        -- set the client
        client = gameClient game
        -- stake
        v      = lovelaceValueOf (fpStake fp)
        -- choice
        c      = fpChoice fp
        -- nonce choice hash
        bs     = sha2_256 $ fpNonce fp `concatenate` if c == Zero then bsZero else bsOne
    -- (runInitialise client) will first mint the nft corrosponding
        -- then create utxo at state machine address to start state machine 
            -- put that nft in that utxo to uniquely identify it
            -- the datum and value of that utxo are given by 
                -- (GameDatum bs Nothing)
                -- v
            -- we use mapError to adjust the error messages
    void $ mapError' $ runInitialise client (GameDatum bs Nothing) v
    logInfo @String $ "made first move: " ++ show (fpChoice fp)
    -- here we tell the last Just threadToken
    tell $ Last $ Just tt

    -- wait for the first deadline to pass
    waitUntilTimeHasPassed $ fpPlayDeadline fp

    -- call getOnChainState on the client
    m <- mapError' $ getOnChainState client
    case m of
        Nothing             -> throwError "game output not found"
        -- we are only interested the o
            -- tyTxOutData is used to access the datum from o
        Just ((o, _), _) -> case tyTxOutData o of
            -- if the second player hasn't moved player 1 reclaims
            GameDatum _ Nothing -> do
                logInfo @String "second player did not play"
                -- runStep creates a tx and submits it that transitions the state machine
                    -- it takes the client and the redeemer
                    -- we get a transition result that tells whether it fails or succeeds and why and what state
                void $ mapError' $ runStep client ClaimFirst
                logInfo @String "first player reclaimed stake"
            -- if they have then reveal
            GameDatum _ (Just c') | c' == c -> do
                logInfo @String "second player played and lost"
                -- here runStep with client and then Reveal with its associated nonce
                void $ mapError' $ runStep client $ Reveal $ fpNonce fp
                logInfo @String "first player revealed and won"
            -- else player 2 won
            _ -> logInfo @String "second player played and won"

data SecondParams = SecondParams
    { spFirst          :: !PubKeyHash -- we pass the player 1 pkh this time
    , spStake          :: !Integer
    , spPlayDeadline   :: !POSIXTime
    , spRevealDeadline :: !POSIXTime
    , spChoice         :: !GameChoice
    , spToken          :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON)

secondGame :: forall w s. SecondParams -> Contract w s Text ()
secondGame sp = do
    -- look up public key and define game structure
    pkh <- pubKeyHash <$> Contract.ownPubKey
    -- we don't need to set the threadToken this time around
    let game   = Game
            { gFirst          = spFirst sp
            , gSecond         = pkh
            , gStake          = spStake sp
            , gPlayDeadline   = spPlayDeadline sp
            , gRevealDeadline = spRevealDeadline sp
            , gToken          = spToken sp
            }
        -- set the client
        client = gameClient game
    -- get the state of the client
    m <- mapError' $ getOnChainState client
    case m of
        Nothing          -> logInfo @String "no running game found"
        -- grabbing o get the datum
        Just ((o, _), _) -> case tyTxOutData o of
            GameDatum _ Nothing -> do
                logInfo @String "running game found"
                -- call runStep on Play to have player 2 make their choice
                void $ mapError' $ runStep client $ Play $ spChoice sp
                logInfo @String $ "made second move: " ++ show (spChoice sp)
                -- wait until the reveal deadline has passed
                waitUntilTimeHasPassed $ spRevealDeadline sp
                -- get the state of the chain
                m' <- mapError' $ getOnChainState client
                case m' of
                    -- if the state is Nothing then player 1 won
                    Nothing -> logInfo @String "first player won"
                    Just _  -> do
                        logInfo @String "first player didn't reveal"
                        -- runStep with client on ClaimSecond to have player 2 claim stake
                        void $ mapError' $ runStep client ClaimSecond
                        logInfo @String "second player won"

            -- otherwise throw error
            _ -> throwError "unexpected datum"

-- define schema and endpoints first and second
type GameSchema = Endpoint "first" FirstParams .\/ Endpoint "second" SecondParams

-- endpoints boilerplate for the contract
endpoints :: Contract (Last ThreadToken) GameSchema Text ()
endpoints = (first `select` second) >> endpoints
  where
    first  = endpoint @"first"  >>= firstGame
    second = endpoint @"second" >>= secondGame
