module Common.Split (splitPairOn) where

import Data.List.Split (splitOn)

splitPairOn :: (Eq a) => [a] -> [a] -> ([a], [a])
splitPairOn separator xs = (left, right)
  where
    splitAtComma = splitOn separator

    readPair (l : r : _) = (l, r)
    readPair _ = error "readPair: Illegal input"

    (left, right) = readPair . splitAtComma $ xs
