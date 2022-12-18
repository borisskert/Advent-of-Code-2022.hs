module Day10.Screen (Screen, empty, draw) where

import Common.Grid (Grid, Position, Value, fromTuple, fromValue, toTuple, toValue, x, y)
import qualified Common.Grid as Grid (empty, insert, subgrid)
import Common.Tuple

data PixelPosition = PixelPosition Int Int deriving (Eq, Ord)

instance Position PixelPosition where
  x (PixelPosition x' _) = x'
  y (PixelPosition _ y') = y'
  toTuple (PixelPosition x' y') = (x', y')
  fromTuple (x', y') = PixelPosition x' y'

screenColumns :: Int
screenColumns = 40

screenLines :: Int
screenLines = 6

fromCycle :: Int -> PixelPosition
fromCycle = fromTuple . flipTuple . (`divMod` screenColumns)

newtype PixelValue = PixelValue Bool deriving (Eq, Ord)

instance Value PixelValue where
  toValue (_, '#') = Just (PixelValue True)
  toValue _ = Just (PixelValue False)
  fromValue (Just (PixelValue True)) = '#'
  fromValue (Just (PixelValue False)) = '.'
  fromValue Nothing = '.'

data Screen = Screen {pixels :: Grid PixelPosition PixelValue, cycles :: Int}

empty :: Screen
empty = Screen {pixels = Grid.empty, cycles = 0}

draw :: Int -> Screen -> Screen
draw spritePosition screen@Screen {pixels = grid, cycles = screenCycles} = Screen {pixels = nextGrid, cycles = screenCycles + 1}
  where
    position = fromCycle screenCycles
    pixel = PixelValue (isLit spritePosition screen)
    nextGrid = Grid.insert position pixel grid

isLit :: Int -> Screen -> Bool
isLit spritePosition Screen {cycles = screenCycles} =
  spritePosition
    `elem` [ ledPosition - 1,
             ledPosition,
             ledPosition + 1
           ]
  where
    ledPosition = (`mod` screenColumns) screenCycles

instance Show Screen where
  show Screen {pixels = grid} = show . Grid.subgrid (fromTuple (0, 0)) (screenColumns, screenLines) $ grid
