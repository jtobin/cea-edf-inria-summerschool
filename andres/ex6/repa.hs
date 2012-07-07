{-# LANGUAGE BangPatterns #-}

import Data.Array.Repa
import Data.Array.Repa.Operators.Traversal (unsafeTraverse2)

rev :: Source r e => Array r DIM1 e -> Array D DIM1 e
rev !v = backpermute (Z :. n) (\(Z :. i) -> (Z :. (n - i - 1))) v
    where n = size $ extent v

-- Help from Fredrik on this one.
cartesian :: (Source r e, Source r1 e1)
           => (e -> e1 -> c)
           -> Array r  DIM1 e
           -> Array r1 DIM1 e1
           -> Array D  DIM2 c
cartesian !f !arr !brr = unsafeTraverse2 arr brr (\(Z :. i) (Z :. j) -> Z :. i :. j) (\g h sh -> f (g (Z :. (i - 1))) (h (Z :. (j - 1)))) 
    where (Z :. i) = extent arr
          (Z :. j) = extent brr

-- Testing

testDim1Array0 :: Array U DIM1 Int
testDim1Array0 = computeUnboxedS $ fromFunction (Z :. 10 :: DIM1) (\(Z :. i) -> i + 1)

testDim1Array1 :: Array U DIM1 Int
testDim1Array1 = computeUnboxedS $ fromFunction (Z :. 1000 :: DIM1) (\(Z :. i) -> i + 1)

testDim2Array0 :: Array U DIM2 Int
testDim2Array0 = computeUnboxedS $ cartesian (+) testDim1Array0 testDim1Array0

testDim2Array1 :: Array D DIM2 Int
testDim2Array1 = cartesian (*) testDim1Array1 testDim1Array1

main = computeUnboxedP testDim2Array1 >>= print



