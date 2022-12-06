{-# LANGUAGE QuasiQuotes #-}

module Day05.StacksOfCrates (StacksOfCrates, readFrom, empty, move, fromList, tops, moveN) where

import Common.MultiStack (MultiStack, popAt, popNAt, pushAt, pushNAt, top, topAt, topNAt)
import qualified Common.MultiStack as MultiStack (empty, fromList)
import Data.Bifunctor (second)
import Data.Maybe (fromJust, isJust)
import Day05.Crate (Crate, readLine)
import Day05.Move
import Text.RE.PCRE.String

type Index = Char

newtype StacksOfCrates = StacksOfCrates (MultiStack Index Crate) deriving (Eq, Show)

empty :: StacksOfCrates
empty = StacksOfCrates MultiStack.empty

fromList :: [(Index, [Crate])] -> StacksOfCrates
fromList = StacksOfCrates . MultiStack.fromList

readFrom :: [String] -> StacksOfCrates
readFrom s = foldl (flip pushAllAt) empty indexedCrates
  where
    indices = map head . matches . (*=~ [re|[0-9]|]) . last $ s
    crates = map readLine . init $ s
    indexedCrates = reverse . map (map (second fromJust) . filter (isJust . snd) . zip indices) $ crates

pushAllAt :: [(Index, Crate)] -> StacksOfCrates -> StacksOfCrates
pushAllAt pairs (StacksOfCrates stacks) =
  StacksOfCrates
    . foldl (\s (index, crate) -> pushAt index crate s) stacks
    $ pairs

move :: Move -> StacksOfCrates -> StacksOfCrates
move m stacks@(StacksOfCrates stack)
  | moveCount > 0 = move nextMove newStacks
  | otherwise = stacks
  where
    moveCount = count m
    fromIndex = from m
    toIndex = to m
    crate = topAt fromIndex stack
    newStacks = StacksOfCrates . pushAt toIndex crate . popAt fromIndex $ stack
    nextMove = moveOf (moveCount - 1) fromIndex toIndex

tops :: StacksOfCrates -> [Crate]
tops (StacksOfCrates stacks) = top stacks

-- CrateMover 9001
moveN :: Move -> StacksOfCrates -> StacksOfCrates
moveN m (StacksOfCrates stacks) = StacksOfCrates newStack
  where
    moveCount = count m
    fromIndex = from m
    toIndex = to m
    crates = topNAt fromIndex moveCount stacks
    newStack = pushNAt toIndex crates . popNAt fromIndex moveCount $ stacks
