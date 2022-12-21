module Common.MultiMap (MultiMap, empty, insert, lookup) where

import Data.Map (Map)
import qualified Data.Map as Map (empty, findWithDefault, insert)
import Data.Set (Set)
import qualified Data.Set as Set (empty, insert, toList)
import Prelude hiding (lookup)

newtype MultiMap k v = MultiMap (Map k (Set v)) deriving (Eq, Show)

empty :: MultiMap k v
empty = MultiMap Map.empty

insert :: (Ord k, Ord v) => k -> v -> MultiMap k v -> MultiMap k v
insert key value (MultiMap myMap) = MultiMap . Map.insert key newList $ myMap
  where
    newList = Set.insert value . Map.findWithDefault Set.empty key $ myMap

lookup :: (Ord k) => k -> MultiMap k v -> [v]
lookup key (MultiMap myMap) = Set.toList . Map.findWithDefault Set.empty key $ myMap
