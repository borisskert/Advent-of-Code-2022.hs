module Day09.RopeBridge (howManyTailPositions) where

import Day09.Field
import Day09.Move

howManyTailPositions :: String -> Int
howManyTailPositions = length . visited . foldl (flip perform) empty . readMany
