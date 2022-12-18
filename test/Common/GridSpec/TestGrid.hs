module Common.GridSpec.TestGrid
  ( TestGrid,
    TestValue,
    toChar,
    fromChar,
    empty,
    fromList,
    toList,
    lookup,
    lookupPositions,
    columns,
    rows,
    allNorthOf,
    allSouthOf,
    allWestOf,
    allEastOf,
    insert,
    subgrid,
  )
where

import Common.Grid
  ( Grid,
    Value,
    fromTuple,
    toTuple,
  )
import qualified Common.Grid as Grid
  ( allEastOf,
    allNorthOf,
    allSouthOf,
    allWestOf,
    columns,
    empty,
    fromList,
    insert,
    lookup,
    lookupPositions,
    rows,
    subgrid,
    toList,
  )
import qualified Common.Grid as GridValue (fromValue, toValue)
import Common.OctaGridPosition
import Data.Bifunctor (bimap)
import Prelude hiding (lookup)

newtype TestValue = TestValue Char deriving (Eq, Show, Ord)

type TestGrid = Grid Position TestValue

instance Value TestValue where
  toValue (_, c) = Just . TestValue $ c
  fromValue (Just (TestValue c)) = c
  fromValue Nothing = '_'

toChar :: TestValue -> Char
toChar (TestValue c) = c

fromChar :: Char -> TestValue
fromChar = TestValue

empty :: Grid Position TestValue
empty = Grid.empty

fromList :: [((Int, Int), Char)] -> Grid Position TestValue
fromList = Grid.fromList . map (bimap fromTuple fromChar)

toList :: Grid Position TestValue -> [((Int, Int), Char)]
toList = map (bimap toTuple toChar) . Grid.toList

lookup :: (Int, Int) -> Grid Position TestValue -> Maybe Char
lookup pos = fmap toChar . Grid.lookup (fromTuple pos)

lookupPositions :: Char -> Grid Position TestValue -> [(Int, Int)]
lookupPositions value grid = map toTuple . (`Grid.lookupPositions` grid) . fromChar $ value

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

insert :: (Int, Int) -> Char -> Grid Position TestValue -> Grid Position TestValue
insert pos value = Grid.insert (fromTuple pos) (TestValue value)

subgrid :: (Int, Int) -> (Int, Int) -> Grid Position TestValue -> Grid Position TestValue
subgrid pos = Grid.subgrid (fromTuple pos)
