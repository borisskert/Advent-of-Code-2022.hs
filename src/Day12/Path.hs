module Day12.Path (Path, empty, singleton, fromList, steps, append) where

import Common.CrossGridPosition (Position)

newtype Path = Path [Position]

instance Eq Path where
  a == b = steps a == steps b

instance Ord Path where
  compare a b = compare (steps a) (steps b)

instance Show Path where
  show (Path xs) = show xs

empty :: Path
empty = Path []

singleton :: Position -> Path
singleton x = Path [x]

fromList :: [Position] -> Path
fromList = Path

steps :: Path -> Int
steps (Path xs) = length xs

append :: Position -> Path -> Path
append pos (Path xs) = Path (xs ++ [pos])
