module Day04.Assignment (Assignment (Assignment), fromTo, readOne, contains) where

import Common.Split (splitPairOn)

data Assignment = Assignment Int Int deriving (Show, Eq)

readOne :: String -> Assignment
readOne s = Assignment (read left) (read right)
  where
    (left, right) = splitPairOn "-" s

fromTo :: Int -> Int -> Assignment
fromTo = Assignment

contains :: Assignment -> Assignment -> Bool
contains (Assignment fromA toA) (Assignment fromB toB) = fromA <= fromB && toA >= toB
