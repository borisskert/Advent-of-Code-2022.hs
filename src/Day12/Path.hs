module Day12.Path (Path, empty, fromList, steps, prepend, contains, isEmpty) where

import Common.CrossGridPosition (Position)

newtype Path = Path [Position] deriving (Eq, Show)

empty :: Path
empty = Path []

fromList :: [Position] -> Path
fromList = Path

steps :: Path -> Int
steps (Path xs) = length xs

prepend :: Position -> Path -> Path
prepend pos (Path xs) = Path (pos : xs)

contains :: Position -> Path -> Bool
contains pos (Path path) = pos `elem` path

isEmpty :: Path -> Bool
isEmpty (Path path) = null path
