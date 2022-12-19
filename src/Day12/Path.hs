module Day12.Path (Path, empty, singleton, fromList, steps, prepend, append, contains, isEmpty, areIntersecting) where

import Common.CrossGridPosition (Position)
import Data.Set (Set)
import qualified Data.Set as Set (empty, fromList, insert, intersection, member, singleton)

data Path = Path [Position] (Set Position)

instance Eq Path where
    a == b = steps a == steps b

instance Ord Path where
    compare a b = compare (steps a) (steps b)

instance Show Path where
  show (Path xs _) = show xs

empty :: Path
empty = Path [] Set.empty

singleton :: Position -> Path
singleton x = Path [x] (Set.singleton x)

fromList :: [Position] -> Path
fromList xs = Path xs (Set.fromList xs)

steps :: Path -> Int
steps (Path xs _) = length xs

prepend :: Position -> Path -> Path
prepend pos (Path xs mySet) = Path (pos : xs) (Set.insert pos mySet)

append :: Position -> Path -> Path
append pos (Path xs mySet) = Path (xs ++ [pos]) (Set.insert pos mySet)

contains :: Position -> Path -> Bool
contains pos (Path _ path) = pos `Set.member` path

isEmpty :: Path -> Bool
isEmpty (Path path _) = null path

areIntersecting :: Path -> Path -> Bool
areIntersecting (Path _ a) (Path _ b) = not . null . Set.intersection a $ b
