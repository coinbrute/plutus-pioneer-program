{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main
    ( main
    ) where

import Control.Concurrent
import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString        (ByteString)
import Data.ByteString.Char8  (unpack)
import Data.Proxy             (Proxy (..))
import Data.Text              (pack)
import Data.UUID
import Network.HTTP.Req
import Text.Regex.TDFA


main :: IO ()
main = do
    -- read the oracle.cid file and bind it
    uuid <- read <$> readFile "oracle.cid"
    -- log it out
    putStrLn $ "oracle contract instance id: " ++ show uuid
    -- then go to go
    go uuid Nothing
  where
    go :: UUID -> Maybe Integer -> IO a
    go uuid m = do
        -- we get the exchange rate
        x <- getExchangeRate
        let y = Just x
        -- check if its changed
        when (m /= y) $
        -- update it
            updateOracle uuid x
        -- make a api delay
        threadDelay 5_000_000
        -- call again with y
        go uuid y

-- this function makes the call to update the oracle
updateOracle :: UUID -> Integer -> IO ()
updateOracle uuid x = runReq defaultHttpConfig $ do
    -- here we prepare a post request
    v <- req
        -- request type
        POST
        -- request endpopint
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "update")
        -- request body
        (ReqBodyJson x)
        -- extras
        (Proxy :: Proxy (JsonResponse ()))
        (port 8080)
    -- lift the IO response and log it
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "updated oracle to " ++ show x
        else "error updating oracle"

-- get the exchange rate
getExchangeRate :: IO Integer
getExchangeRate = runReq defaultHttpConfig $ do
    -- construct the api request
    v <- req
        -- METHOD
        GET
        -- ENDPOINT
        (https "coinmarketcap.com" /: "currencies" /: "cardano")
        -- BODY
        NoReqBody
        --RESPONSE
        bsResponse
        -- EXTRAS
        mempty
        -- REGEX for matching price values in the response body
    let priceRegex      = "priceValue___11gHJ \">\\$([\\.0-9]*)" :: ByteString
        (_, _, _, [bs]) = responseBody v =~ priceRegex :: (ByteString, ByteString, ByteString, [ByteString])
        d               = read $ unpack bs :: Double
        x               = round $ 1_000_000 * d
    -- liftIO and log message
    liftIO $ putStrLn $ "queried exchange rate: " ++ show d
    return x
