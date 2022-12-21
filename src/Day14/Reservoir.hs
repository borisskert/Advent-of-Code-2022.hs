module Day14.Reservoir (Reservoir, toList, insertScans, empty, rock) where

import Common.Grid (Grid, Value)
import qualified Common.Grid as Grid (empty, fromTuple, fromValue, insert, toList, toValue)
import Common.OctaGridPosition (Position)
import Day14.RockScan (RockScan, deflate)
import Day14.RockScans (RockScans)
import qualified Day14.RockScans as RockScans (toList)

data Material = Sand | Rock deriving (Eq, Show, Ord)

instance Value Material where
  toValue = undefined
  fromValue (Just Sand) = 'o'
  fromValue (Just Rock) = '#'
  fromValue Nothing = '.'

rock :: Material
rock = Rock

newtype Reservoir = Reservoir (Grid Position Material) deriving (Show, Eq)

empty :: Reservoir
empty = Reservoir Grid.empty

insertScans :: RockScans -> Reservoir -> Reservoir
insertScans rs reservoir = foldl (flip insertScan) reservoir . RockScans.toList $ rs

insertScan :: RockScan -> Reservoir -> Reservoir
insertScan rs (Reservoir grid) = foldl insertScan' (Reservoir grid) . map (Grid.fromTuple) $ (deflate rs)
  where
    insertScan' :: Reservoir -> (Position) -> Reservoir
    insertScan' (Reservoir grid) (position) = Reservoir . Grid.insert position rock $ grid

toList :: Reservoir -> [(Position, Material)]
toList (Reservoir grid) = Grid.toList grid
