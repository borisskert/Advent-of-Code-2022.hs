module Common.GridSpec.TestGrid
  ( TestGrid,
    TestValue,
    toChar,
    fromChar,
    empty,
    fromLines,
    fromList,
    toList,
    lookup,
    columns,
    rows,
    allNorthOf,
    allSouthOf,
    allWestOf,
    allEastOf,
  )
where

import Common.Grid (Grid, GridValue)
import qualified Common.Grid as Grid
  ( allEastOf,
    allNorthOf,
    allSouthOf,
    allWestOf,
    columns,
    empty,
    fromLines,
    fromList,
    lookup,
    rows,
    toList,
  )
import qualified Common.Grid as GridValue (fromValue, toValue)
import Common.OctaGridPosition
import Prelude hiding (lookup)

newtype TestValue = TestValue Char deriving (Eq, Show)

type TestGrid = Grid Position TestValue

instance GridValue TestValue where
  toValue (_, c) = Just . TestValue $ c
  fromValue (Just (TestValue c)) = c
  fromValue Nothing = '_'

toChar :: TestValue -> Char
toChar (TestValue c) = c

fromChar :: Char -> TestValue
fromChar = TestValue

empty :: Grid Position TestValue
empty = Grid.empty

fromLines :: String -> Grid Position TestValue
fromLines = Grid.fromLines

fromList :: [[Char]] -> Grid Position TestValue
fromList = Grid.fromList . map (map fromChar)

toList :: Grid Position TestValue -> [[Char]]
toList = Grid.toList

lookup :: (Int, Int) -> Grid Position TestValue -> Maybe Char
lookup pos = fmap toChar . Grid.lookup (fromTuple pos)

columns :: Grid Position TestValue -> [[(Int, Int)]]
columns = map (map toTuple) . Grid.columns

rows :: Grid Position TestValue -> [[(Int, Int)]]
rows = map (map toTuple) . Grid.rows

allNorthOf :: (Int, Int) -> Grid Position TestValue -> [(Int, Int)]
allNorthOf pos grid = map toTuple . (`Grid.allNorthOf` grid) . fromTuple $ pos

allSouthOf :: (Int, Int) -> Grid Position TestValue -> [(Int, Int)]
allSouthOf pos grid = map toTuple . (`Grid.allSouthOf` grid) . fromTuple $ pos

allWestOf :: (Int, Int) -> Grid Position TestValue -> [(Int, Int)]
allWestOf pos grid = map toTuple . (`Grid.allWestOf` grid) . fromTuple $ pos

allEastOf :: (Int, Int) -> Grid Position TestValue -> [(Int, Int)]
allEastOf pos grid = map toTuple . (`Grid.allEastOf` grid) . fromTuple $ pos
