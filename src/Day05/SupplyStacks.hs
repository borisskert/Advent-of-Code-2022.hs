module Day05.SupplyStacks (topOfEachStack) where

import Common.Split
import Day05.Crate
import Day05.Move
import Day05.StacksOfCrates

topOfEachStack :: String -> [Char]
topOfEachStack input = map name . tops . foldl (flip move) stacks $ moves
  where
    (stackLines, moveLines) = splitPairOn [""] . lines $ input
    stacks = readFrom stackLines
    moves = map readOne moveLines
