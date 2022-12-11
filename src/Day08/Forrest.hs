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

import Common.CrossGrid (CrossGrid, allEastOf, allNorthOf, allSouthOf, allWestOf, columns, rows, elems)
import qualified Common.CrossGrid as CrossGrid (fromLines, fromList, lookup, mapGrid)
import Common.List (distinctOn, groupOn, takeAscendingOn, takeUntil)
import Data.Char (digitToInt)
import Data.Maybe (mapMaybe)
import Debug.Trace (traceShow)

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
allTrees (Forrest grid )= elems grid

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
scenicScore (Tree pos _) forrest =
  product . map length $
    [ visibleNorthOf pos forrest,
      visibleSouthOf pos forrest,
      visibleWestOf pos forrest,
      visibleEastOf pos forrest
    ]

visibleNorthOf :: Position -> Forrest -> [Tree]
visibleNorthOf pos (Forrest grid) = visibleUntil maxHeight . mapMaybe (`CrossGrid.lookup` grid) . allNorthOf pos $ grid
  where
    maxHeight = maybe 0 height . CrossGrid.lookup pos $ grid

visibleSouthOf :: Position -> Forrest -> [Tree]
visibleSouthOf pos (Forrest grid) = visibleUntil maxHeight . mapMaybe (`CrossGrid.lookup` grid) . allSouthOf pos $ grid
  where
    maxHeight = maybe 0 height . CrossGrid.lookup pos $ grid

visibleWestOf :: Position -> Forrest -> [Tree]
visibleWestOf pos (Forrest grid) = visibleUntil maxHeight . mapMaybe (`CrossGrid.lookup` grid) . allWestOf pos $ grid
  where
    maxHeight = maybe 0 height . CrossGrid.lookup pos $ grid

visibleEastOf :: Position -> Forrest -> [Tree]
visibleEastOf pos (Forrest grid) = visibleUntil maxHeight . mapMaybe (`CrossGrid.lookup` grid) . allEastOf pos $ grid
  where
    maxHeight = maybe 0 height . CrossGrid.lookup pos $ grid

visibleUntil :: Height -> [Tree] -> [Tree]
visibleUntil maxHeight = takeUntil ((>= maxHeight) . height)
