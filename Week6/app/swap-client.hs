{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
    ( main
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class                  (MonadIO (..))
import Data.Aeson                              (Result (..), fromJSON)
import Data.Monoid                             (Last (..))
import Data.Proxy                              (Proxy (..))
import Data.Text                               (pack)
import Data.UUID
import Ledger.Value                            (flattenValue)
import Network.HTTP.Req
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse (..))
import Plutus.PAB.Webserver.Types
import System.Environment                      (getArgs)
import System.IO
import Text.Read                               (readMaybe)

import Week6.Oracle.PAB                       (OracleContracts)


main :: IO ()
main = do
    -- get the wallet number to be able to grab correct file
    [i :: Int] <- map read <$> getArgs
    -- load uuid through file
    uuid       <- read <$> readFile ('W' : show i ++ ".cid")
    hSetBuffering stdout NoBuffering
    -- log message 
    putStrLn $ "swap contract instance id for Wallet " ++ show i ++ ": " ++ show uuid
    -- go to go with uuid
    go uuid
  where
    go :: UUID -> IO a
    go uuid = do
        -- get thing to perform
        cmd <- readCommand
        case cmd of
            Offer amt -> offer uuid amt
            Retrieve  -> retrieve uuid
            Use       -> use uuid
            Funds     -> getFunds uuid
        -- call recursively
        go uuid

    -- takes a command to perfom and returns the command bound 
    readCommand :: IO Command
    readCommand = do
        putStr "enter command (Offer amt, Retrieve, Use or Funds): "
        s <- getLine
        maybe readCommand return $ readMaybe s

-- data type for Command used above for the input from user
data Command = Offer Integer | Retrieve | Use | Funds
    deriving (Show, Read, Eq, Ord)

-- get funds availability
getFunds :: UUID -> IO ()
getFunds uuid = handle h $ runReq defaultHttpConfig $ do
    -- call funds endpoint
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "funds")
        (ReqBodyJson ())
        (Proxy :: Proxy (JsonResponse ()))
        (port 8080)
    if responseStatusCode v /= 200
        then liftIO $ putStrLn "error getting funds"
        else do
            -- get the status of the endpoint since funds returns the state
            w <- req
                GET
                (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "status")
                NoReqBody
                (Proxy :: Proxy (JsonResponse (ContractInstanceClientState OracleContracts)))
                (port 8080)
            -- parse the json and get the funds
            liftIO $ putStrLn $ case fromJSON $ observableState $ cicCurrentState $ responseBody w of
                Success (Last (Just f)) -> "funds: " ++ show (flattenValue f)
                _                       -> "error decoding state"
  where
    h :: HttpException -> IO ()
    h _ = threadDelay 1_000_000 >> getFunds uuid

-- offer endpoint to offer swap amount 
offer :: UUID -> Integer -> IO ()
offer uuid amt = handle h $ runReq defaultHttpConfig $ do
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "offer")
        -- here we have the amount to pass in in the request body
        (ReqBodyJson amt)
        (Proxy :: Proxy (JsonResponse ()))
        (port 8080)
    -- lift and log the response
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "offered swap of " ++ show amt ++ " lovelace"
        else "error offering swap"
  where
    h :: HttpException -> IO ()
    h _ = threadDelay 1_000_000 >> offer uuid amt

-- retrieve is prettry straightforward to retrieve the funds in a swaps utxos
retrieve :: UUID -> IO ()
retrieve uuid = handle h $ runReq defaultHttpConfig $ do
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "retrieve")
        (ReqBodyJson ())
        (Proxy :: Proxy (JsonResponse ()))
        (port 8080)
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "retrieved swaps"
        else "error retrieving swaps"
  where
    h :: HttpException -> IO ()
    h _ = threadDelay 1_000_000 >> retrieve uuid

-- use is very similar to retrieve in its construction
use :: UUID -> IO ()
use uuid = handle h $ runReq defaultHttpConfig $ do
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "use")
        (ReqBodyJson ())
        (Proxy :: Proxy (JsonResponse ()))
        (port 8080)
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "used swap"
        else "error using swap"
  where
    h :: HttpException -> IO ()
    h _ = threadDelay 1_000_000 >> use uuid

-- each of these have a thread delay of 1_000_000 milliseconds 
