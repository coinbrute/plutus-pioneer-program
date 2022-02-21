module Week4.Writer where

import Control.Monad
import Week4.Monad

-- this is a type that takes a thing of type (a) and a list of Strings i.e. log messages
  -- it derives the Show type
data Writer a = Writer a [String]
    deriving Show

-- we have a number function to write a number logger
  -- it will log out the number given
number :: Int -> Writer Int
number n = Writer n $ ["number: " ++ show n]

-- takes a list of log messages and return a writer unit
tell :: [String] -> Writer ()
tell = Writer ()

-- this does a similar thing to the other foo functions
  -- it will take three Writer Ints and sum the Ints and concat the log messages also writing out the sum of them 
foo :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo (Writer k xs) (Writer l ys) (Writer m zs) =
  let
    s = k + l + m -- the sum to show in the log message
    Writer _ us = tell ["sum: " ++ show s]
  in
    Writer s $ xs ++ ys ++ zs ++ us

-- bind a function to a Writer to create a new Writer with the function applied
bindWriter :: Writer a -> (a -> Writer b) -> Writer b
bindWriter (Writer a xs) f =
  let
    Writer b ys = f a
  in
    Writer b $ xs ++ ys

-- foo rewritten with bindWriter applied in lambda syntax
foo' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo' x y z = x `bindWriter` \k -> -- bind result k with the next function run
             y `bindWriter` \l -> -- so on and so forth
             z `bindWriter` \m ->
             let s = k + l + m -- sum them all to s
             in tell ["sum: " ++ show s] `bindWriter` \_ ->
                Writer s []

-- use threeInts in the monad module to sum x/y/z and produce s call tell on the log message and return s
foo'' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo'' x y z = do
  s <- threeInts x y z
  tell ["sum: " ++ show s]
  return s

-- below is the standard pattern for implementing Functor and Applicative
instance Functor Writer where
    fmap = liftM

instance Applicative Writer where
    pure = return
    (<*>) = ap

-- make Writer an instance of monad so it can use the threeInts function
instance Monad Writer where
    return a = Writer a []
    (>>=) = bindWriter
