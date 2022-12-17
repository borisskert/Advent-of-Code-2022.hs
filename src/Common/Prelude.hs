module Common.Prelude (isMultipleOf) where

isMultipleOf :: (Integral a) => a -> a -> Bool
isMultipleOf a b = a `mod` b == 0
