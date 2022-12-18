module Common.MultiMap (MultiMap, empty, insert, lookup) where

import Common.List
import Data.Map (Map)
import qualified Data.Map as Map (empty, findWithDefault, insert)
import Prelude hiding (lookup)

newtype MultiMap k v = MultiMap (Map k [v]) deriving (Eq, Show)

empty :: MultiMap k v
empty = MultiMap Map.empty

insert :: (Ord k) => k -> v -> MultiMap k v -> MultiMap k v
insert key value (MultiMap myMap) = MultiMap . Map.insert key newList $ myMap
  where
    newList = append value . Map.findWithDefault [] key $ myMap

lookup :: (Ord k) => k -> MultiMap k v -> [v]
lookup key (MultiMap myMap) = Map.findWithDefault [] key myMap
