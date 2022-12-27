module Common.Math (gauss, gaussBetween) where

-- Sum of the first n natural numbers
gauss :: Integral a => a -> a
gauss n = n * (n + 1) `div` 2

-- Sum of all natural numbers between a and b
gaussBetween :: Integral a => a -> a -> a
gaussBetween a b = gauss b - gauss a + a
