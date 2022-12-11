module Day08.Forrest (Forrest, readFrom, fromList, tree, visibleFromNorth, visibleFromWest, visibleFromSouth, visibleFromEast, visibleTrees) where

import Common.CrossGrid (CrossGrid, columns, rows)
import qualified Common.CrossGrid as CrossGrid (fromLines, fromList, lookup, mapGrid)
import Common.List (distinctOn, takeAscendingOn)
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
