import Test.QuickCheck

import Data.List hiding (sort)
import qualified Data.List as List

import Funcs

sortIsIdempotent xs = sort (sort xs) == sort xs
sortMatchesDataListSort xs = sort xs == List.sort xs

everyListIsSorted :: Ord a => [a] -> Bool
everyListIsSorted xs = sorted xs

main = 
    mapM_ quickCheck [sortIsIdempotent, sortMatchesDataListSort]

 



