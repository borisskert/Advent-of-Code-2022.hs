module Day09.RopeBridge (howManyTailPositions) where

import Day09.Motion
import Day09.Simulation

howManyTailPositions :: Int -> String -> Int
howManyTailPositions size = length . visited . foldl (flip perform) (create size) . readMany
