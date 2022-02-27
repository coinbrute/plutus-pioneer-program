{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Spec.Model
    ( tests
    , test
    , TSModel (..)
    )  where

import           Control.Lens                       hiding (elements)
import           Control.Monad                      (void, when)
import           Data.Default                       (Default (..))
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (isJust, isNothing)
import           Data.Monoid                        (Last (..))
import           Data.String                        (IsString (..))
import           Data.Text                          (Text)
import           Plutus.Contract.Test
import           Plutus.Contract.Test.ContractModel
import           Plutus.Trace.Emulator
import           Ledger                             hiding (singleton)
import           Ledger.Ada                         as Ada
import           Ledger.Value
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Week8.TokenSale                   (TokenSale (..), TSStartSchema, TSUseSchema, startEndpoint, useEndpoints)

-- here we use Tasty and QuickCheck to provide testing for our Plutus TokenSale contract

-- define the state of one token sale instance used for the model
data TSState = TSState
    { _tssPrice    :: !Integer -- current price
    , _tssLovelace :: !Integer -- current supply of lovelace
    , _tssToken    :: !Integer -- current supply of token
    } deriving Show

makeLenses ''TSState

-- define model for use in the tests
    -- map from wallet to token sale state
    -- have two wallets each running a token sale contract each trading different tokens
newtype TSModel = TSModel {_tsModel :: Map Wallet TSState}
    deriving Show

makeLenses ''TSModel

-- this is Tasty's way of integrating with QuickCheck by taking a string and a quickcheck property 
tests :: TestTree
tests = testProperty "token sale model" prop_TS

-- provide the instance for the contract model
instance ContractModel TSModel where

    -- here we have an associated data type in our instance
        -- this represents the actions Quickcheck takes
    data Action TSModel =
              Start Wallet -- this wallet starts a ts contract
            | SetPrice Wallet Wallet Integer -- second wallet sets price for ts operated by first wallet at integer price
            | AddTokens Wallet Wallet Integer -- second wallet adds tokens to first wallets token sale at integer amount
            | Withdraw Wallet Wallet Integer Integer -- second wallet withdraws integer tokens and integer ada from first wallet
            | BuyTokens Wallet Wallet Integer -- second wallet buys from first wallet token sale integer token amount
        deriving (Show, Eq)

    -- this is associated data type in our instance 
        -- this represents each instance of the contract key
    data ContractInstanceKey TSModel w s e where
        StartKey :: Wallet           -> ContractInstanceKey TSModel (Last TokenSale) TSStartSchema Text
        UseKey   :: Wallet -> Wallet -> ContractInstanceKey TSModel ()               TSUseSchema   Text

    -- given a key we just defined get a tag for it
        -- this gives us a tag for the start key and the use keys
    instanceTag key _ = fromString $ "instance tag for: " ++ show key

    -- tell the system how to generate a random action
    arbitraryAction _ = oneof $ 
        -- oneof is a combinator provided by qc. so given a list of choices it picks one and runs it
        -- (<*>) this is appicative in monadic terms. it means runs the first thing first then the next thing

        -- so here we pick wallet 1 or 2 then pass it to start
        (Start <$> genWallet) :
        -- here we pick 2 random wallets and a non negative number then pass it to SetPrice
        [ SetPrice  <$> genWallet <*> genWallet <*> genNonNeg ]               ++
        -- same here but pass to AddTokens
        [ AddTokens <$> genWallet <*> genWallet <*> genNonNeg ]               ++
        -- same here but to BuyTokens
        [ BuyTokens <$> genWallet <*> genWallet <*> genNonNeg ]               ++
        -- same here but with three wallets and to Withdraw
        [ Withdraw  <$> genWallet <*> genWallet <*> genNonNeg <*> genNonNeg ]

    -- get the initial state of our model which is the map of wallet to no token sale 
    initialState = TSModel Map.empty

    -- this changes the state of the model based on the start action
    nextState (Start w) = do
        -- create entry in the map for value (w)
            -- focus in with a lens 
                -- at wallet w zoom in to the model
                -- set value to TSState 0 0 0 <- this is the token sale with 0 tokens 0 lovelace and 0 price
            -- wait 1 step/slot/block etc
        (tsModel . at w) $= Just (TSState 0 0 0)
        wait 1

    -- change the state of the model based on the SetPrice action
    nextState (SetPrice v w p) = do
        -- when both wallets match
            -- set the tssPrice to p by using lens function (ix) to traverse tsModel for wallet (v)
            -- ix will traverse and look for the wallet 
            -- wait 1 step
        when (v == w) $
            (tsModel . ix v . tssPrice) $= p
        wait 1

    -- change the state of the model based on the AddTokens action
    nextState (AddTokens v w n) = do
        -- bind the result of the hasStarted call on the first wallet to started
        started <- hasStarted v                                     -- has the token sale started?
        -- when the amount to add is positive and token sale has started proceed
        when (n > 0 && started) $ do
            -- first we get the balance change from the second wallet
            -- then we view that which is a zoom in from optics
            -- and ask the model state
            bc <- askModelState $ view $ balanceChange w
            -- this is the token that wallet v is selling
            let token = tokens Map.! v
            -- now we calc whether w has enough
            when (tokenAmt + assetClassValueOf bc token >= n) $ do  -- does the wallet have the tokens to give?
                -- withdraw will move n tokens from w 
                withdraw w $ assetClassValue token n
                -- use tsModel to and ix to traverse in with v and get tssToken and update n on value tssToken
                    -- so they were removed from w and showed up in the model
                (tsModel . ix v . tssToken) $~ (+ n)
        wait 1

    -- so w wants to buy n tokens from v
    -- 
    nextState (BuyTokens v w n) = do
        -- make shure n - positive
        when (n > 0) $ do
            -- make sure token sale is started
            m <- getTSState v
            case m of
                Just t
                    -- lens zoom into the model and look at the token amount and see if we have enough tokens to buy
                    | t ^. tssToken >= n -> do
                        -- lens zoom into the price
                        let p = t ^. tssPrice
                            -- purchase price is p * n
                            l = p * n
                        -- withdraw l from w
                        withdraw w $ lovelaceValueOf l
                        -- deposit n tokens from v into w
                        deposit w $ assetClassValue (tokens Map.! v) n
                        -- lens traverse with ix using v into the lovelace and update with l
                        (tsModel . ix v . tssLovelace) $~ (+ l)
                        -- lens traverse with ix use v into the token and update with sutracting n
                        (tsModel . ix v . tssToken)    $~ (+ (- n))
                -- else return out
                _ -> return ()
        wait 1

    -- wallet w wants to withdraw n token and l lovelace
    nextState (Withdraw v w n l) = do
        -- v must equal w
        when (v == w) $ do
            -- token sale must be started
            m <- getTSState v
            case m of
                Just t
                    -- check there are at least n token and l lovelace
                    | t ^. tssToken >= n && t ^. tssLovelace >= l -> do
                        -- deposit amount l into w and token n from w
                        deposit w $ lovelaceValueOf l <> assetClassValue (tokens Map.! w) n
                        -- make updates
                        (tsModel . ix v . tssLovelace) $~ (+ (- l))
                        (tsModel . ix v . tssToken) $~ (+ (- n))
                -- else return unit
                _ -> return ()
        wait 1

    -- here we make the link to our endpoints
        -- it takes a HandleFun state which gives access to the contract handles
    perform h _ cmd = case cmd of
        -- if the action is Start do this
            -- given a StartKey w it returns a handle
            -- this we can provide to the start endpoint along with the other parameters
        (Start w)          -> callEndpoint @"start"      (h $ StartKey w) (tokenCurrencies Map.! w, tokenNames Map.! w, False) >> delay 1
        -- in the same format for the rest we call the UseKey function with the parameters to get the handle then use the existing parameters to reach the endpoint
        (SetPrice v w p)   -> callEndpoint @"set price"  (h $ UseKey v w) p                                                    >> delay 1
        (AddTokens v w n)  -> callEndpoint @"add tokens" (h $ UseKey v w) n                                                    >> delay 1
        (BuyTokens v w n)  -> callEndpoint @"buy tokens" (h $ UseKey v w) n                                                    >> delay 1
        (Withdraw v w n l) -> callEndpoint @"withdraw"   (h $ UseKey v w) (n, l)                                               >> delay 1

    -- these say whether certain actions can be done before a certain time. 
        -- think random generatino of tests before you want them 
    -- Start can not be done once the token sale has started
    -- The others are not possible if things are already going
    precondition s (Start w)          = isNothing $ getTSState' s w
    precondition s (SetPrice v _ _)   = isJust    $ getTSState' s v
    precondition s (AddTokens v _ _)  = isJust    $ getTSState' s v
    precondition s (BuyTokens v _ _)  = isJust    $ getTSState' s v
    precondition s (Withdraw v _ _ _) = isJust    $ getTSState' s v

-- derivations of Eq and Show for contract instance keys
deriving instance Eq (ContractInstanceKey TSModel w s e)
deriving instance Show (ContractInstanceKey TSModel w s e)

-- this looks at a model state and a wallet and returns the model state
getTSState' :: ModelState TSModel -> Wallet -> Maybe TSState
-- taking the modelState we zoom in with lens into the contractstate where we can get the tsModel we defined which takes a model which gets a map which we get give the key to get the value from 
getTSState' s v = s ^. contractState . tsModel . at v

-- we use our first helper function to get the state of the given wallet
getTSState :: Wallet -> Spec TSModel (Maybe TSState)
getTSState v = do
    s <- getModelState
    return $ getTSState' s v

-- we use the second helper function using the wallet and if its a just we return true
hasStarted :: Wallet -> Spec TSModel Bool
hasStarted v = isJust <$> getTSState v

-- wallets
w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2

-- list of wallets
wallets :: [Wallet]
wallets = [w1, w2]

-- list for currency symbols
tokenCurrencies :: Map Wallet CurrencySymbol
tokenCurrencies = Map.fromList $ zip wallets ["aa", "bb"]

-- list for token names
tokenNames :: Map Wallet TokenName
tokenNames = Map.fromList $ zip wallets ["A", "B"]

-- specify the tokens as a map
tokens :: Map Wallet AssetClass
tokens = Map.fromList [(w, AssetClass (tokenCurrencies Map.! w, tokenNames Map.! w)) | w <- wallets]

-- this the token sale itself defined with the sellers wallets and token amounts.
    -- leaving out the thread token as the testing framework tracks everything
tss :: Map Wallet TokenSale
tss = Map.fromList
    [ (w, TokenSale { tsSeller = pubKeyHash $ walletPubKey w
                    , tsToken  = tokens Map.! w
                    , tsTT     = Nothing
                    })
    | w <- wallets
    ]

-- specify wait in slots
delay :: Int -> EmulatorTrace ()
delay = void . waitNSlots . fromIntegral

-- here we link our contract instances to our contract instance specs we want to run. 
    -- we specify the key the wallet and the associated contract
instanceSpec :: [ContractInstanceSpec TSModel]
instanceSpec =
    [ContractInstanceSpec (StartKey w) w startEndpoint | w <- wallets] ++
    -- for this one we look up the token sale belonging to v since it is the parameter for the useEndpoint
    [ContractInstanceSpec (UseKey v w) w $ useEndpoints $ tss Map.! v | v <- wallets, w <- wallets]

-- chooses arbitrary wallet for action
genWallet :: Gen Wallet
genWallet = elements wallets

-- chooses non negative number
genNonNeg :: Gen Integer
genNonNeg = getNonNegative <$> arbitrary

-- speicify the initial distribution for the tokens on startup
tokenAmt :: Integer
tokenAmt = 1_000

-- this is the link between all the work above and QuickCheck
    -- it takes an Actions and return the Property
prop_TS :: Actions TSModel -> Property
-- it takes the number of instances we want to run along with the run actions with options 
prop_TS = withMaxSuccess 100 . propRunActionsWithOptions
    -- here we say we are using the custom wallet distributions along with the default check options
    (defaultCheckOptions & emulatorConfig .~ EmulatorConfig (Left d) def def)
    instanceSpec -- provide the instance specs above
    (const $ pure True)
  where
    d :: InitialDistribution
    d = Map.fromList $ [ ( w
                         , lovelaceValueOf 1_000_000_000 <>
                           mconcat [assetClassValue t tokenAmt | t <- Map.elems tokens])
                       | w <- wallets
                       ]

-- run the test with quickcheck and the properties
test :: IO ()
test = quickCheck prop_TS
