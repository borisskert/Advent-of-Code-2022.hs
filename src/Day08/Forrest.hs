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

import Common.CrossGrid (CrossGrid, allEastOf, allNorthOf, allSouthOf, allWestOf, columns, elems, rows)
import qualified Common.CrossGrid as CrossGrid (fromLines, fromList, lookup, mapGrid)
import Common.List (distinctOn, takeAscendingOn, takeUntil)
import Data.Char (digitToInt)
import Data.Maybe (mapMaybe)

type Position = (Int, Int)

type Height = Int

data Tree = Tree Position Int deriving (Eq, Show)

newtype Forrest = Forrest (CrossGrid Tree) deriving (Eq, Show)

readFrom :: String -> Forrest
readFrom = Forrest . CrossGrid.fromLines (\(pos, c) -> Just . tree pos . digitToInt $c)

fromList :: [[Int]] -> Forrest
fromList = Forrest . CrossGrid.mapGrid Tree . CrossGrid.fromList

tree :: Position -> Height -> Tree
tree = Tree

allTrees :: Forrest -> [Tree]
allTrees (Forrest grid) = elems grid

visibleFromNorth :: Forrest -> [[Tree]]
visibleFromNorth (Forrest grid) = map (visible . mapMaybe (`CrossGrid.lookup` grid)) . columns $ grid

visibleFromWest :: Forrest -> [[Tree]]
visibleFromWest (Forrest grid) = map (visible . mapMaybe (`CrossGrid.lookup` grid)) . rows $ grid

visibleFromSouth :: Forrest -> [[Tree]]
visibleFromSouth (Forrest grid) = map (visible . mapMaybe (`CrossGrid.lookup` grid) . reverse) . columns $ grid

visibleFromEast :: Forrest -> [[Tree]]
visibleFromEast (Forrest grid) = map (visible . mapMaybe (`CrossGrid.lookup` grid) . reverse) . rows $ grid

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

visibleFrom :: (Position -> CrossGrid Tree -> [Position]) -> Tree -> Forrest -> [Tree]
visibleFrom getTrees ourTree (Forrest grid) =
  visibleUntil maxHeight
    . mapMaybe (`CrossGrid.lookup` grid)
    . getTrees pos
    $ grid
  where
    maxHeight = height ourTree
    pos = position ourTree

visibleUntil :: Height -> [Tree] -> [Tree]
visibleUntil maxHeight = takeUntil ((>= maxHeight) . height)
