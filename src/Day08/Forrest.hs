module Day08.Forrest
  ( Forrest,
    readFrom,
    fromList,
    tree,
    allTrees,
    visibleFromNorth,
    visibleFromWest,
    visibleFromSouth,
    visibleFromEast,
    visibleTrees,
    scenicScore,
  )
where

import Common.Grid (Grid, Value, allEastOf, allNorthOf, allSouthOf, allWestOf, columns, elems, fromTuple, fromValue, rows, toTuple, toValue)
import qualified Common.Grid as Grid (fromLines, fromList, lookup, mapGrid)
import Common.List (distinctOn, takeAscendingOn, takeUntil)
import Common.OctaGridPosition
import Data.Char (digitToInt)
import Data.Maybe (mapMaybe)

type Height = Int

data Tree = Tree Position Int deriving (Eq, Show)

instance Value Tree where
  toValue (_, '_') = Nothing
  toValue (pos, c) = Just . tree (toTuple pos) . digitToInt $c
  fromValue (Just (Tree _ h)) = head . show $ h
  fromValue Nothing = '_'

newtype Forrest = Forrest (Grid Position Tree) deriving (Eq, Show)

readFrom :: String -> Forrest
readFrom = Forrest . Grid.fromLines

fromList :: [[Int]] -> Forrest
fromList = Forrest . Grid.mapGrid Tree . Grid.fromList

tree :: (Int, Int) -> Height -> Tree
tree = Tree . fromTuple

allTrees :: Forrest -> [Tree]
allTrees (Forrest grid) = elems grid

visibleFromNorth :: Forrest -> [[Tree]]
visibleFromNorth (Forrest grid) = map (visible . mapMaybe (`Grid.lookup` grid)) . columns $ grid

visibleFromWest :: Forrest -> [[Tree]]
visibleFromWest (Forrest grid) = map (visible . mapMaybe (`Grid.lookup` grid)) . rows $ grid

visibleFromSouth :: Forrest -> [[Tree]]
visibleFromSouth (Forrest grid) = map (visible . mapMaybe (`Grid.lookup` grid) . reverse) . columns $ grid

visibleFromEast :: Forrest -> [[Tree]]
visibleFromEast (Forrest grid) = map (visible . mapMaybe (`Grid.lookup` grid) . reverse) . rows $ grid

height :: Tree -> Height
height (Tree _ h) = h

visible :: [Tree] -> [Tree]
visible = takeAscendingOn height

position :: Tree -> Position
position (Tree p _) = p

visibleTrees :: Forrest -> [Tree]
visibleTrees forrest =
  distinctOn position . concatMap concat $
    [ visibleFromNorth forrest,
      visibleFromSouth forrest,
      visibleFromWest forrest,
      visibleFromEast forrest
    ]

scenicScore :: Tree -> Forrest -> Int
scenicScore ourTree forrest =
  product . map length $
    [ visibleFrom allNorthOf ourTree forrest,
      visibleFrom allSouthOf ourTree forrest,
      visibleFrom allWestOf ourTree forrest,
      visibleFrom allEastOf ourTree forrest
    ]

visibleFrom :: (Position -> Grid Position Tree -> [Position]) -> Tree -> Forrest -> [Tree]
visibleFrom getTrees ourTree (Forrest grid) =
  visibleUntil maxHeight
    . mapMaybe (`Grid.lookup` grid)
    . getTrees pos
    $ grid
  where
    maxHeight = height ourTree
    pos = position ourTree

visibleUntil :: Height -> [Tree] -> [Tree]
visibleUntil maxHeight = takeUntil ((>= maxHeight) . height)
