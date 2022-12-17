module Common.Prelude (isMultipleOf, lcmOf) where

isMultipleOf :: (Integral a) => a -> a -> Bool
isMultipleOf a b = a `mod` b == 0

lcmOf :: (Integral a) => [a] -> a
lcmOf [] = 0
lcmOf [x] = x
lcmOf (a : b : xs) = lcmOf (lcm a b : xs)
