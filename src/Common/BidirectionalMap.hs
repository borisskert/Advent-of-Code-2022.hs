module Common.BidirectionalMap
  ( BidirectionalMap,
    empty,
    lookup,
    find,
    lookupKey,
    insert,
    insertMissing,
    toList,
    fromList,
    keys,
    elems,
    member,
  )
where

import Common.MultiMap (MultiMap)
import qualified Common.MultiMap as MultiMap (empty, insert, lookup)
import Data.Map (Map)
import qualified Data.Map as Map (elems, empty, insert, keys, lookup, member, toList)
import Data.Maybe (fromJust)
import Prelude hiding (lookup)

data BidirectionalMap k v = BidirectionalMap (Map k v) (MultiMap v k) deriving (Eq, Show)

empty :: BidirectionalMap k v
empty = BidirectionalMap Map.empty MultiMap.empty

fromList :: (Ord k, Ord v) => [(k, v)] -> BidirectionalMap k v
fromList = foldl (\m (k, v) -> insert k v m) empty

toList :: BidirectionalMap k v -> [(k, v)]
toList (BidirectionalMap myMap _) = Map.toList myMap

lookup :: (Ord k) => k -> BidirectionalMap k v -> Maybe v
lookup k (BidirectionalMap myMap _) = Map.lookup k myMap

find :: (Ord k) => k -> BidirectionalMap k v -> v
find k (BidirectionalMap myMap _) = fromJust . Map.lookup k $ myMap

member :: (Ord k) => k -> BidirectionalMap k v -> Bool
member k (BidirectionalMap myMap _) = Map.member k myMap

lookupKey :: (Ord v) => v -> BidirectionalMap k v -> [k]
lookupKey value (BidirectionalMap _ reverseMap) = MultiMap.lookup value reverseMap

insert :: (Ord k, Ord v) => k -> v -> BidirectionalMap k v -> BidirectionalMap k v
insert k v (BidirectionalMap myMap myReverseMap) = BidirectionalMap newMap newReverseMap
  where
    newMap = Map.insert k v myMap
    newReverseMap = MultiMap.insert v k myReverseMap

insertMissing :: (Ord k, Ord v) => k -> v -> BidirectionalMap k v -> BidirectionalMap k v
insertMissing pos value biMap@(BidirectionalMap myMap myReverseMap)
  | Map.member pos myMap = biMap
  | otherwise = BidirectionalMap newMap newReverseMap
  where
    newMap = Map.insert pos value myMap
    newReverseMap = MultiMap.insert value pos myReverseMap

keys :: BidirectionalMap k v -> [k]
keys (BidirectionalMap myMap _) = Map.keys myMap

elems :: BidirectionalMap k v -> [v]
elems (BidirectionalMap myMap _) = Map.elems myMap
