module Day05.SupplyStacks (topOfEachStack, topOfEachStackCrateMover9001) where

import Common.Split
import Day05.Crate
import Day05.Move
import Day05.StacksOfCrates

topOfEachStack :: String -> [Char]
topOfEachStack input = map name . tops . foldl (flip move) stacks $ moves
  where
    (stacks, moves) = readInput input

topOfEachStackCrateMover9001 :: String -> [Char]
topOfEachStackCrateMover9001 input = map name . tops . foldl (flip moveN) stacks $ moves
  where
    (stacks, moves) = readInput input

readInput :: String -> (StacksOfCrates, [Move])
readInput input = (stacks, moves)
  where
    (stackLines, moveLines) = splitPairOn [""] . lines $ input
    stacks = readFrom stackLines
    moves = map readOne moveLines
