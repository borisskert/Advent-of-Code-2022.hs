module Day09.RopeBridge (howManyTailPositions) where

import Day09.Move
import qualified Day09.Snake as Snake

howManyTailPositions :: Int -> String -> Int
howManyTailPositions size = length . Snake.visited . foldl (flip Snake.perform) (Snake.create size) . readMany
