module Week4.Maybe where

-- readMaybe allows you to maybe parse a string into another type or return Nothing
import Text.Read (readMaybe)
import Week4.Monad

-- this function should be able to parse three Strings into Ints. if one fails parsingthe return Nothing
foo :: String -> String -> String -> Maybe Int
-- pretty straightforward just 3 cases checking for readMaybe valeus for x y and z if they all pass add the Just values together else return nothing each step of the way.
foo x y z = case readMaybe x of
    Nothing -> Nothing
    Just k  -> case readMaybe y of
        Nothing -> Nothing
        Just l  -> case readMaybe z of
            Nothing -> Nothing
            Just m  -> Just (k + l + m)

-- take an (a) of Maybe and a function of (a to Maybe b) and return a (b) of Maybe
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
-- if (a) is nothing then return nothing
bindMaybe Nothing  _ = Nothing
-- if (a) is a Just then return the (a) with function applied
bindMaybe (Just x) f = f x

-- here is foo above with bindMaybe applied
foo' :: String -> String -> String -> Maybe Int
foo' x y z = readMaybe x `bindMaybe` \k -> 
             readMaybe y `bindMaybe` \l ->
             readMaybe z `bindMaybe` \m ->
             Just (k + l + m)
{-
foo "3" "4" "5" = readMaybe "3" `bindMaybe` \3 ->
                  readMaybe "4" `bindMaybe` \4 ->
                  readMaybe "5" `bindMaybe` \5 ->
                  Just (3 + 4 + 5)
-}


-- this version uses a function written in a companion module Monad that binds three Maybe Integers
foo'' :: String -> String -> String -> Maybe Int
foo'' x y z = threeInts (readMaybe x) (readMaybe y) (readMaybe z)
