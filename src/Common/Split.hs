module Common.Split (splitPairOn) where

import Data.List.Split (splitOn)

splitPairOn :: (Eq a) => [a] -> [a] -> ([a], [a])
splitPairOn separator xs = (left, right)
  where
    splitAtSeparator = splitOn separator

    readPair (l : r : _) = (l, r)
    readPair _ = error "splitPairOn.readPair: Illegal input"

    (left, right) = readPair . splitAtSeparator $ xs
