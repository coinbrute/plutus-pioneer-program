module Week4.Either where

import Text.Read (readMaybe)
import Week4.Monad

-- this is similar to Maybe but this assigns either left or right values to the output so you can assign values to each
-- this is useful for converting Maybe values to useful error exception handling values
readEither :: Read a => String -> Either String a
readEither s = case readMaybe s of
    Nothing -> Left $ "can't parse: " ++ s
    Just a  -> Right a

-- now we can handle the Maybe like before but as an either
foo :: String -> String -> String -> Either String Int
foo x y z = case readEither x of
    Left err -> Left err
    Right k  -> case readEither y of
        Left err -> Left err
        Right l  -> case readEither z of
            Left err -> Left err
            Right m  -> Right (k + l + m)

-- same here as an either
bindEither :: Either String a -> (a -> Either String b) -> Either String b
bindEither (Left err) _ = Left err
bindEither (Right x)  f = f x

-- and again but simpler with the lambda syntax
foo' :: String -> String -> String -> Either String Int
foo' x y z = readEither x `bindEither` \k ->
             readEither y `bindEither` \l ->
             readEither z `bindEither` \m ->
             Right (k + l + m)

-- and again with the extraction of the Monad types
foo'' :: String -> String -> String -> Either String Int
foo'' x y z = threeInts (readEither x) (readEither y) (readEither z)
