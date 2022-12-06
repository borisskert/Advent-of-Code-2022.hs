module Day05.StacksOfCrates (StacksOfCrates, readFrom, empty, move, fromList, tops, moveN) where

import Common.Stack (Stack, pop, popN, push, pushN, top, topN)
import qualified Common.Stack as Stack (empty, fromList)
import Data.Bifunctor (second)
import qualified Data.Map as Map (Map, elems, empty, fromList, insert, lookup)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Day05.Crate (Crate, readLine)
import Day05.Move

type Index = Char

newtype StacksOfCrates = StacksOfCrates (Map.Map Index (Stack Crate)) deriving (Eq, Show)

empty :: StacksOfCrates
empty = StacksOfCrates Map.empty

readFrom :: [String] -> StacksOfCrates
readFrom s = foldl (flip pushAllAt) empty indexedCrates
  where
    indices = readIndicesFrom . last $ s
    crates = map readLine . init $ s
    indexedCrates = reverse . map (map (second fromJust) . filter (isJust . snd) . zip indices) $ crates

pushAt :: Index -> Crate -> StacksOfCrates -> StacksOfCrates
pushAt index crate (StacksOfCrates stacks) = StacksOfCrates newMap
  where
    newStack = push crate . fromMaybe Stack.empty . Map.lookup index $ stacks
    newMap = Map.insert index newStack stacks

pushAllAt :: [(Index, Crate)] -> StacksOfCrates -> StacksOfCrates
pushAllAt pairs stacks = foldl (\s (index, crate) -> pushAt index crate s) stacks pairs

fromList :: [(Index, [Crate])] -> StacksOfCrates
fromList = StacksOfCrates . Map.fromList . map (second Stack.fromList)

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
popAt index (StacksOfCrates stacks) = StacksOfCrates newMap
  where
    newStack = pop . fromMaybe Stack.empty . Map.lookup index $ stacks
    newMap = Map.insert index newStack stacks

topAt :: Index -> StacksOfCrates -> Crate
topAt index (StacksOfCrates stacks) = top . fromMaybe Stack.empty . Map.lookup index $ stacks

tops :: StacksOfCrates -> [Crate]
tops (StacksOfCrates stacks) = map top . Map.elems $ stacks

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
topNAt index count' (StacksOfCrates stacks) = topN count' . fromMaybe Stack.empty . Map.lookup index $ stacks

pushNAt :: Index -> [Crate] -> StacksOfCrates -> StacksOfCrates
pushNAt index crates (StacksOfCrates stacks) = StacksOfCrates newMap
  where
    newStack = pushN crates . fromMaybe Stack.empty . Map.lookup index $ stacks
    newMap = Map.insert index newStack stacks

popNAt :: Index -> Int -> StacksOfCrates -> StacksOfCrates
popNAt index count' (StacksOfCrates stacks) = StacksOfCrates newMap
  where
    newStack = popN count' . fromMaybe Stack.empty . Map.lookup index $ stacks
    newMap = Map.insert index newStack stacks
