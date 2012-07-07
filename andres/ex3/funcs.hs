module Funcs where

import Test.QuickCheck
import Data.List (permutations)

sort :: [Int] -> [Int]
sort []     = []
sort (x:xs) = insert x (sort xs)

insert :: Int -> [Int] -> [Int]
insert x []                 = [x]
insert x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : insert x ys

-- Testing

(f `preserves` p) x = p x == p (f x)
(f `ensures` p)   x = p (f x)
f `permutes` xs     = f xs `elem` permutations xs

sortPreservesLength = sort `preserves` length
idPreservesLength = id `preserves` length
sortEnsuresSorted = sort `ensures` sorted
sortPermutes xs   = sort `permutes` xs

sorted :: Ord a => [a]-> Bool
sorted []       = True
sorted (x:[])   = True
sorted (x:y:ys) = x <= y && sorted (y:ys)


