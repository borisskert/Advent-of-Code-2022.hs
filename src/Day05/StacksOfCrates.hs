module Day05.StacksOfCrates (StacksOfCrates, readFrom, empty, move, fromList, tops, moveN) where

import Common.MultiStack (MultiStack)
import qualified Common.MultiStack as MultiStack
import Data.Bifunctor (second)
import Data.Maybe (fromJust, isJust)
import Day05.Crate (Crate, readLine)
import Day05.Move

type Index = Char

newtype StacksOfCrates = StacksOfCrates (MultiStack Index Crate) deriving (Eq, Show)

empty :: StacksOfCrates
empty = StacksOfCrates MultiStack.empty

fromList :: [(Index, [Crate])] -> StacksOfCrates
fromList = StacksOfCrates . MultiStack.fromList

readFrom :: [String] -> StacksOfCrates
readFrom s = foldl (flip pushAllAt) empty indexedCrates
  where
    indices = readIndicesFrom . last $ s
    crates = map readLine . init $ s
    indexedCrates = reverse . map (map (second fromJust) . filter (isJust . snd) . zip indices) $ crates

pushAllAt :: [(Index, Crate)] -> StacksOfCrates -> StacksOfCrates
pushAllAt pairs stacks = foldl (\s (index, crate) -> pushAt index crate s) stacks pairs

pushAt :: Index -> Crate -> StacksOfCrates -> StacksOfCrates
pushAt index crate (StacksOfCrates stacks) = StacksOfCrates . MultiStack.pushAt index crate $ stacks

readIndicesFrom :: String -> [Index]
readIndicesFrom [] = []
readIndicesFrom xs
  | null leftTrimmed = []
  | otherwise = (head leftTrimmed :) . readIndicesFrom . tail $ leftTrimmed
  where
    leftTrimmed = dropWhile (== ' ') xs

move :: Move -> StacksOfCrates -> StacksOfCrates
move m stacks
  | moveCount > 0 = move nextMove newStacks
  | otherwise = stacks
  where
    moveCount = count m
    fromIndex = from m
    toIndex = to m
    crate = topAt fromIndex stacks
    newStacks = pushAt toIndex crate . popAt fromIndex $ stacks
    nextMove = moveOf (moveCount - 1) fromIndex toIndex

popAt :: Index -> StacksOfCrates -> StacksOfCrates
popAt index (StacksOfCrates stacks) = StacksOfCrates . MultiStack.popAt index $ stacks

topAt :: Index -> StacksOfCrates -> Crate
topAt index (StacksOfCrates stacks) = MultiStack.topAt index stacks

tops :: StacksOfCrates -> [Crate]
tops (StacksOfCrates stacks) = MultiStack.tops stacks

-- CrateMover 9001
moveN :: Move -> StacksOfCrates -> StacksOfCrates
moveN m stacks = newStacks
  where
    moveCount = count m
    fromIndex = from m
    toIndex = to m
    crates = topNAt fromIndex moveCount stacks
    newStacks = pushNAt toIndex crates . popNAt fromIndex moveCount $ stacks

topNAt :: Index -> Int -> StacksOfCrates -> [Crate]
topNAt index count' (StacksOfCrates stacks) = MultiStack.topNAt index count' stacks

pushNAt :: Index -> [Crate] -> StacksOfCrates -> StacksOfCrates
pushNAt index crates (StacksOfCrates stacks) = StacksOfCrates . MultiStack.pushNAt index crates $ stacks

popNAt :: Index -> Int -> StacksOfCrates -> StacksOfCrates
popNAt index count' (StacksOfCrates stacks) = StacksOfCrates . MultiStack.popNAt index count' $ stacks
