module Common.MultiStack (MultiStack, empty, fromList, pushAt, popAt, topAt, top, topNAt, pushNAt, popNAt) where

import Common.Stack (Stack)
import qualified Common.Stack as Stack
import Data.Bifunctor (second)
import Data.Map (Map)
import qualified Data.Map as Map (elems, empty, fromList, insert, lookup)
import Data.Maybe (fromMaybe)

newtype MultiStack k v = MultiStack (Map k (Stack v)) deriving (Eq, Show)

empty :: MultiStack k v
empty = MultiStack Map.empty

fromList :: (Ord k) => [(k, [v])] -> MultiStack k v
fromList = MultiStack . Map.fromList . map (second Stack.fromList)

pushAt :: (Ord k) => k -> v -> MultiStack k v -> MultiStack k v
pushAt index crate (MultiStack stacks) = MultiStack newMap
  where
    newStack = Stack.push crate . fromMaybe Stack.empty . Map.lookup index $ stacks
    newMap = Map.insert index newStack stacks

popAt :: (Ord k) => k -> MultiStack k v -> MultiStack k v
popAt index (MultiStack stacks) = MultiStack newMap
  where
    newStack = Stack.pop . fromMaybe Stack.empty . Map.lookup index $ stacks
    newMap = Map.insert index newStack stacks

topAt :: (Ord k) => k -> MultiStack k v -> v
topAt index (MultiStack stacks) = Stack.top . fromMaybe Stack.empty . Map.lookup index $ stacks

top :: MultiStack k v -> [v]
top (MultiStack stacks) = map Stack.top . Map.elems $ stacks

topNAt :: (Ord k) => k -> Int -> MultiStack k v -> [v]
topNAt index count (MultiStack stacks) = Stack.topN count . fromMaybe Stack.empty . Map.lookup index $ stacks

pushNAt :: (Ord k) => k -> [v] -> MultiStack k v -> MultiStack k v
pushNAt index crates (MultiStack stacks) = MultiStack newMap
  where
    newStack = Stack.pushN crates . fromMaybe Stack.empty . Map.lookup index $ stacks
    newMap = Map.insert index newStack stacks

popNAt :: (Ord k) => k -> Int -> MultiStack k v -> MultiStack k v
popNAt index count (MultiStack stacks) = MultiStack newMap
  where
    newStack = Stack.popN count . fromMaybe Stack.empty . Map.lookup index $ stacks
    newMap = Map.insert index newStack stacks
