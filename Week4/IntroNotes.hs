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
