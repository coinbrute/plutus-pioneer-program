module Week8.QuickCheck where

import Test.QuickCheck

prop_simple :: Bool
prop_simple = 2 + 2 == (4 :: Int)

-- Insertion sort code:

-- | Sort a list of integers in ascending order.
--
-- >>> sort [5,1,9]
-- [1,5,9]
--
sort :: [Int] -> [Int] -- not correct
sort []     =  []
sort (x:xs) =  insert x $ sort xs

-- | Insert an integer at the right position into an /ascendingly sorted/
-- list of integers.
--
-- >>> insert 5 [1,9]
-- [1,5,9]
--
insert :: Int -> [Int] -> [Int] -- not correct
insert x []                     =  [x]
insert x (y:ys)  | x <= y       =  x : y : ys
                 | otherwise    =  y : insert x ys

isSorted :: [Int] -> Bool
isSorted []           = True
isSorted [_]          = True
isSorted (x : y : ys) = x <= y && isSorted (y : ys)

-- these quick check properties check functions we write for completeness and correctness
 -- quickcheck will generate random values for the input values and brute it against it for a certain amount it will also attempt to shrink a failure counter when encountered. 
prop_sort_sorts :: [Int] -> Bool
prop_sort_sorts xs = isSorted $ sort xs

-- this one we say the property being sorted should preserve its length
 -- this way quick check wont attempt to shrink the list down on failures
prop_sort_preserves_length :: [Int] -> Bool
prop_sort_preserves_length xs = length (sort xs) == length xs

-- now this is just a quick check way to test properties on functions but not necessarily the end all be all testing solution but a good starting approach and still more powerful than traditional unit testing 
 -- this is due to its large and automated and randomized approach

