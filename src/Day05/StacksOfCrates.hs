module Day05.StacksOfCrates (StacksOfCrates, readFrom, empty, pushAt) where

import Common.Stack (Stack, push)
import qualified Common.Stack as Stack (empty)
import Data.Bifunctor (second)
import qualified Data.Map as Map (Map, empty, insert, lookup)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Day05.Crate (Crate, readLine)

type Index = Char

newtype StacksOfCrates = StacksOfCrates (Map.Map Index (Stack Crate)) deriving (Eq, Show)

empty :: StacksOfCrates
empty = StacksOfCrates Map.empty

readFrom :: String -> StacksOfCrates
readFrom s = foldl (flip pushAllAt) empty indexedCrates
  where
    indices = readIndicesFrom . last . lines $ s
    crates = map readLine . init . lines $ s
    indexedCrates = reverse . map (map (second fromJust) . filter (isJust . snd) . zip indices) $ crates

pushAt :: Index -> Crate -> StacksOfCrates -> StacksOfCrates
pushAt index crate (StacksOfCrates stacks) = StacksOfCrates newMap
  where
    newStack = push crate . fromMaybe Stack.empty . Map.lookup index $ stacks
    newMap = Map.insert index newStack stacks

pushAllAt :: [(Index, Crate)] -> StacksOfCrates -> StacksOfCrates
pushAllAt pairs stacks = foldl (\s (index, crate) -> pushAt index crate s) stacks pairs

readIndicesFrom :: String -> [Index]
readIndicesFrom [] = []
readIndicesFrom xs
  | null leftTrimmed = []
  | otherwise = (head leftTrimmed :) . readIndicesFrom . tail $ leftTrimmed
  where
    leftTrimmed = dropWhile (== ' ') xs
