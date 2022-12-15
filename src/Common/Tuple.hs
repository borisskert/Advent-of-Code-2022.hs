module Common.Tuple (flipTuple) where

flipTuple :: (a, a) -> (a, a)
flipTuple (x, y) = (y, x)
