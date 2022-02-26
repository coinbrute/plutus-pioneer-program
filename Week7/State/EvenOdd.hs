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

module Week7.EvenOdd
    ( Game (..)
    , GameChoice (..)
    , FirstParams (..)
    , SecondParams (..)
    , GameSchema
    , endpoints
    ) where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (FromJSON, ToJSON)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Ledger.Value
import           Playground.Contract  (ToSchema)
import           Plutus.Contract      as Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Prelude              (Semigroup (..), Show (..), String)
import qualified Prelude

-- this is the Alice and Bob game implementation

data Game = Game
    { gFirst          :: !PubKeyHash -- alice
    , gSecond         :: !PubKeyHash -- bob
    , gStake          :: !Integer -- amount to put in for the round in lovelace
    , gPlayDeadline   :: !POSIXTime -- deadline for other player to enter
    , gRevealDeadline :: !POSIXTime -- deadline for winner to show their hand
    , gToken          :: !AssetClass -- arbitrary nft used to identify the right instance of the utxo to keep track of where we are in the game
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Game

-- data type for which type of move players can make
data GameChoice = Zero | One
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

-- instance of Eq of GameChoice 
instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    Zero == Zero = True
    One  == One  = True
    _    == _    = False

PlutusTx.unstableMakeIsData ''GameChoice

-- data type for the datum of the Game 
    -- we use a ByteString and a maybe GameChoice since there is a chance it will be the first move and the second player hasnt gone yet
data GameDatum = GameDatum ByteString (Maybe GameChoice)
    deriving Show

-- we make our datum an instance of Eq 
    -- we say they are equal if both components are equal
instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')

PlutusTx.unstableMakeIsData ''GameDatum

-- we make a custom redeemer
    -- Play is when the second player moves
    -- Reveal is when the first player reveals
    -- ClaimFirst is when the second player doesn't make a move
    -- ClaimSecond is when the first player doesn't reveal
data GameRedeemer = Play GameChoice | Reveal ByteString | ClaimFirst | ClaimSecond
    deriving Show

PlutusTx.unstableMakeIsData ''GameRedeemer

-- given a value get the lovelaces contained
{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

-- given an output tx and function converting a datum hash to a Maybe datum return a GameDatum
{-# INLINABLE gameDatum #-}
gameDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe GameDatum
gameDatum o f = do
    -- this takes the output tx and returns the datum hash if there is one 
    dh      <- txOutDatum o
    -- we then apply the function supplied and retrieve the datum from the hash
    Datum d <- f dh
    -- and from that datum we derive a GameDatum by passing it as such 
    PlutusTx.fromBuiltinData d

-- here we make our validator for the core business logic
    -- we pass our Game 
    -- then we pass two ByteStrings for the zero and one
    -- then the datum redeemer and context
{-# INLINABLE mkGameValidator #-}
mkGameValidator :: Game -> ByteString -> ByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsZero' bsOne' dat red ctx =
    -- the input im validating must be identified by the state token
    traceIfFalse "token missing from input" (assetClassValueOf (txOutValue ownInput) (gToken game) == 1) &&
    case (dat, red) of
        -- first player moved second player moving
        (GameDatum bs Nothing, Play c) ->
            -- this move is being made by the second player
            traceIfFalse "not signed by second player"   (txSignedBy info (gSecond game))                                   &&
            -- the first player put the stake down for the game
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            -- the second player puts his stake down
            traceIfFalse "second player's stake missing" (lovelaces (txOutValue ownOutput) == (2 * gStake game))            &&
            -- the output datum must have the move along with it
            traceIfFalse "wrong output datum"            (outputDatum == GameDatum bs (Just c))                             &&
            -- it must be before the first deadline
            traceIfFalse "missed deadline"               (to (gPlayDeadline game) `contains` txInfoValidRange info)         &&
            -- the nft must be passed on to the new utxo to identify the new state
            traceIfFalse "token missing from output"     (assetClassValueOf (txOutValue ownOutput) (gToken game) == 1)

        -- both players move payer 1 wins 
        (GameDatum bs (Just c), Reveal nonce) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (gFirst game))                                    &&
            -- nonce must match using the checkNonce function
            traceIfFalse "commit mismatch"               (checkNonce bs nonce c)                                            &&
            -- must be before second deadline
            traceIfFalse "missed deadline"               (to (gRevealDeadline game) `contains` txInfoValidRange info)       &&
            -- there should be 2 stakes in the pot to pay out
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            -- send the nft back to player 1
            traceIfFalse "NFT must go to first player"   nftToFirst

        -- second player never moves first player claims back their stake
        (GameDatum _ Nothing, ClaimFirst) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (gFirst game))                                    &&
            -- ensure deadline is met
            traceIfFalse "too early"                     (from (1 + gPlayDeadline game) `contains` txInfoValidRange info)   &&
            -- check for the stake
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            -- send the nft to player 1
            traceIfFalse "NFT must go to first player"   nftToFirst

        -- player 1 never reveals after player 2 makes their move leading to player 2 collecitng both stakes
        (GameDatum _ (Just _), ClaimSecond) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (gSecond game))                                   &&
            -- ensure deadline is met
            traceIfFalse "too early"                     (from (1 + gRevealDeadline game) `contains` txInfoValidRange info) &&
            -- there should be 2 stakes in the pot to pay out
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            -- send the nft to player 1
            traceIfFalse "NFT must go to first player"   nftToFirst

        _                              -> False
  where
      -- get tx info from script context
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- find the input currently being validated from the script context
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "game input missing"
        Just i  -> txInInfoResolved i

    -- check all outputs going to same address and succeed with only one returning that one
    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one game output"

    -- give GameDatum for our ownOutput
    outputDatum :: GameDatum
    outputDatum = case gameDatum ownOutput (`findDatum` info) of
        Nothing -> traceError "game output datum not found"
        Just d  -> d

    -- using hash submitted and the nonce provided 
        -- check the moves to see if nonce is valid 
    checkNonce :: ByteString -> ByteString -> GameChoice -> Bool
    -- concat nonce with the gamechoice in bytstring form
        -- apply the hash function to the concatted bystring and check equality
    checkNonce bs nonce cSecond = sha2_256 (nonce `concatenate` cFirst) == bs
      where
        cFirst :: ByteString
        -- take the game coice and convert to bytestring
        cFirst = case cSecond of
            Zero -> bsZero'
            One  -> bsOne'
    -- this nft identifies the correct utxo and if will go back to the play who originated the game since they put it in play in the first place
    nftToFirst :: Bool
    -- we check the value paid to on info from the first player in the game and get the token 
    nftToFirst = assetClassValueOf (valuePaidTo info $ gFirst game) (gToken game) == 1

-- helper type to bundle info about datum and redeemer
data Gaming
instance Scripts.ValidatorTypes Gaming where
    type instance DatumType Gaming = GameDatum
    type instance RedeemerType Gaming = GameRedeemer

-- bystrings for zero and one
bsZero, bsOne :: ByteString
bsZero = "0"
bsOne  = "1"

-- parameterized validator compiler function for game
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

-- this gets the game at the contract output
findGameOutput :: Game -> Contract w s Text (Maybe (TxOutRef, TxOutTx, GameDatum))
findGameOutput game = do
    -- the utxos at the game address
    utxos <- utxoAt $ gameAddress game
    return $ do
        -- on the predicate f find the utxo that satisfies it returning a Maybe of that
        (oref, o) <- find f $ Map.toList utxos
        -- check whether the output from the pair above contains our token
            -- get the datum from it
        dat       <- gameDatum (txOutTxOut o) (`Map.lookup` txData (txOutTxTx o))
        return (oref, o, dat)
  where
    f :: (TxOutRef, TxOutTx) -> Bool
    f (_, o) = assetClassValueOf (txOutValue $ txOutTxOut o) (gToken game) == 1

-- gets a time and waits until time has passed and we are in next slot
waitUntilTimeHasPassed :: AsContractError e => POSIXTime -> Contract w s e ()
waitUntilTimeHasPassed t = do
    -- get current slot and log
    s1 <- currentSlot
    logInfo @String $ "current slot: " ++ show s1 ++ ", waiting until " ++ show t
    -- get time and wait amount
    void $ awaitTime t >> waitNSlots 1
    -- get current slot and log
    s2 <- currentSlot
    logInfo @String $ "waited until: " ++ show s2

-- data type for the parameters of the first game
data FirstParams = FirstParams
    { fpSecond         :: !PubKeyHash
    , fpStake          :: !Integer
    , fpPlayDeadline   :: !POSIXTime
    , fpRevealDeadline :: !POSIXTime
    , fpNonce          :: !ByteString
    , fpCurrency       :: !CurrencySymbol
    , fpTokenName      :: !TokenName
    , fpChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- the first round
firstGame :: forall w s. FirstParams -> Contract w s Text ()
firstGame fp = do
    -- get our public key
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let game = Game
            { gFirst          = pkh -- we are player one
            , gSecond         = fpSecond fp -- use input params for this
            , gStake          = fpStake fp -- same
            , gPlayDeadline   = fpPlayDeadline fp -- same
            , gRevealDeadline = fpRevealDeadline fp -- same 
            , gToken          = AssetClass (fpCurrency fp, fpTokenName fp) -- same
            }
        v    = lovelaceValueOf (fpStake fp) <> assetClassValue (gToken game) 1 -- asset stake plus nft
        c    = fpChoice fp -- our choice
        bs   = sha2_256 $ fpNonce fp `concatenate` if c == Zero then bsZero else bsOne -- this is the hash of our choice and nonce
        tx   = Constraints.mustPayToTheScript (GameDatum bs Nothing) v -- here we pay to the script
    -- submit tx
    ledgerTx <- submitTxConstraints (typedGameValidator game) tx
    -- await confirmation and log
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "made first move: " ++ show (fpChoice fp)

    -- wait until first deadline passed
    waitUntilTimeHasPassed $ fpPlayDeadline fp

    -- find our game utxo
    m   <- findGameOutput game
    -- get current time
    now <- currentTime
    case m of
        Nothing             -> throwError "game output not found"
        -- grab the datum 
        Just (oref, o, dat) -> case dat of
            -- player 2 hasn't moved
            GameDatum _ Nothing -> do
                logInfo @String "second player did not play"
                -- we must provide the utxo and the validator
                let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (gameValidator game)
                    -- we must spend this utxo with this redeemer
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ClaimFirst) <>
                              Constraints.mustValidateIn (from now)
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ txId ledgerTx'
                logInfo @String "reclaimed stake"

            GameDatum _ (Just c') | c' == c -> do
                logInfo @String "second player played and lost"
                -- specify utxo and game validator
                let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (gameValidator game)
                    -- we must spend the script with reveal nonce redeemer and do so before the reveal deadline passes
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Reveal $ fpNonce fp) <>
                              Constraints.mustValidateIn (to $ now + 1000)
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ txId ledgerTx'
                logInfo @String "victory"

            _ -> logInfo @String "second player played and won"

-- data type for the game parameters of the second game
data SecondParams = SecondParams
    { spFirst          :: !PubKeyHash
    , spStake          :: !Integer
    , spPlayDeadline   :: !POSIXTime
    , spRevealDeadline :: !POSIXTime
    , spCurrency       :: !CurrencySymbol
    , spTokenName      :: !TokenName
    , spChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

secondGame :: forall w s. SecondParams -> Contract w s Text ()
secondGame sp = do
    -- find our pkh to define game value
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let game = Game
            { gFirst          = spFirst sp
            , gSecond         = pkh -- we are second player this time
            , gStake          = spStake sp
            , gPlayDeadline   = spPlayDeadline sp
            , gRevealDeadline = spRevealDeadline sp
            , gToken          = AssetClass (spCurrency sp, spTokenName sp)
            }
    -- find utxo that contains game nft
    m <- findGameOutput game
    case m of
        -- we found a game running we we haven't moved yet
        Just (oref, o, GameDatum bs Nothing) -> do
            logInfo @String "running game found"
            now <- currentTime
            -- token is the nft from the game
            let token   = assetClassValue (gToken game) 1
            -- x is local for the stake in lovelace and we put in twice stake plus the nft
            let v       = let x = lovelaceValueOf (spStake sp) in x <> x <> token
                -- c is our choice
                c       = spChoice sp
                -- provide the utxo and validator and script instance
                lookups = Constraints.unspentOutputs (Map.singleton oref o)                                   <>
                          Constraints.otherScript (gameValidator game)                                        <>
                          Constraints.typedValidatorLookups (typedGameValidator game)
                -- must spend existing utxo with redeemer play 
                -- must create new utxo with updated datum 
                -- also our new stake
                -- all before deadline
                tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Play c) <>
                          Constraints.mustPayToTheScript (GameDatum bs $ Just c) v                            <>
                          Constraints.mustValidateIn (to now)
            ledgerTx <- submitTxConstraintsWith @Gaming lookups tx
            let tid = txId ledgerTx
            void $ awaitTxConfirmed tid
            logInfo @String $ "made second move: " ++ show (spChoice sp)

            -- wait until reveal deadline has passed
            waitUntilTimeHasPassed $ spRevealDeadline sp

            -- find new utxo
            m'   <- findGameOutput game
            now' <- currentTime
            case m' of
                -- now first player won
                Nothing             -> logInfo @String "first player won"
                Just (oref', o', _) -> do
                    -- first player never revealed
                    logInfo @String "first player didn't reveal"
                    -- must have utxo and validator
                    let lookups' = Constraints.unspentOutputs (Map.singleton oref' o')                                     <>
                                   Constraints.otherScript (gameValidator game)
                        -- we must spend the utxo we found
                        -- must do after reveal deadline has passed
                        -- must hand back nft to player 1
                        tx'      = Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData ClaimSecond) <>
                                   Constraints.mustValidateIn (from now')                                                  <>
                                   Constraints.mustPayToPubKey (spFirst sp) token
                    ledgerTx' <- submitTxConstraintsWith @Gaming lookups' tx'
                    void $ awaitTxConfirmed $ txId ledgerTx'
                    logInfo @String "second player won"

        _ -> logInfo @String "no running game found"

-- define game schema with endpoints for first and second
type GameSchema = Endpoint "first" FirstParams .\/ Endpoint "second" SecondParams

-- contract endpoints that just recursively offers choice between endpoints 
endpoints :: Contract () GameSchema Text ()
endpoints = (first `select` second) >> endpoints
  where
    first  = endpoint @"first"  >>= firstGame
    second = endpoint @"second" >>= secondGame
