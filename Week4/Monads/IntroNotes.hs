-- in java because we have no control of what happens inside functions the outputs may differ between function calls

{-
public static in foo() {=
 ...
}
... foo()
... foo()
-}

-- this means we can't replace a variable with a call to foo expecting the call to replace it with the same value each time

-- in haskell it would be something like 
{-
foo ::Int
foo = ...
-}

-- now we can always be sure that calling foo will always result in the same output no matter the function body
-- makes testing easier since you just have to check if a function returns an expected value since no side effects occur

-- though side effects are necessary for effective programs

-- so to do that we use IO 
-- it is a built in primitive that gives you the recipe to invoke side effects with whatever type constructors you are giving it to result with the output type it is designated with
-- so 
 -- foo :: IO Int
 -- will result in an IO of type Int when evaluated
-- the only way to execute an IO is in a main function during the haskell runtime or in the repl

-- if you are not returning any side effects you can return a unit. 
-- just outputting something you can return IO ()
 -- string output could be this type i.e. hello-world

-- in the case of another common one getLine its type output is IO String so it waits for some input and if there are side effects i.e. the input then it will output that input as type String to the local variable in the main. or to the repl as a String

-- we can also use the Functor class on IO invocations 
-- something like toUpper 'q' to a string gotten from the command line
-- fmap (map toUpper) getLine
-- so this would map over each character in the string inputted and map toUpper to each string and fmap that IO to the IO needed

-- (>>) is a sequence operator to do things in sequence for IO
 -- this ignores the result of the first

-- (>>=) this is a bind operator that is similar to sequence but does not discard the first result. It applies the result of the first to the type of the second and outputs the result
 -- getLine >>= putStrLn
 -- this would output the string gotten from getLine

-- (return) outuputs results immediately without producing side effects using a type IO of input type

{-
main :: IO ()
main = bar 

bar :: IO ()
bar = getLine >>= \s -> -- bind the gotten string to lambda var s
      getLine >>= \t -> -- bind the gotten string to lambda var t
      putStrLn (s ++ t) -- output both concatted 
-}

-- similarly to IO the Maybe Either and Writer class each also have bind or side effect functions to handle things in the class like IO
-- below are some of them 

-- (>>=)      :: IO a            -> (a -> IO b)            -> IO b
-- bindMaybe  :: Maybe a         -> (a -> Maybe b)         -> Maybe b
-- bindEither :: Either String a -> (a -> Either String b) -> Either String b
-- bindWriter :: Writer a        -> (a -> Writer b)        -> Writer b

-- you can also have the option to return something with no side effects
-- these are those options for these modules

-- return              :: a -> IO a
-- Just                :: a -> Maybe a
-- Right               :: a -> Either String a
-- (\a -> Writer a []) :: a -> Writer a

-- The big thing to take away from monad is its a type that has two main features Bind and Return 
      -- bind allows you to take some type and apply something to it to return some other type
            -- it combines the computations together
            -- if the first computation fails the whole thing fails
                  -- this is dependant on the monad though
      -- retun allows you to return something that could potentially fail
            -- a writer can log a message out or it may not. 
            -- a maybe can maybe give a value or it can give nothing 
            -- an either can give either one or the other
-- a monad is a overloader it allows encompassing abilities to be contained in a typeclass of monad with overarching abilities to be used or required
-- "do" notation is widely used in monadic computation
      -- just syntactic sugar and operates similarly to lambda bindings by binding output of one function to the next and operating in a linear fashion from one call to the next if one fails per monad type rules the whole may fail or may not.
            -- the left of the (<-) is what the result is bound to
-- "let" notation is often used as well within a do block
      -- the "in" keyword is not required in a do block
      -- used to bind a specific keyword 
