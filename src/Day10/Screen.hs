module Day10.Screen (Screen, empty, draw, toLines) where

import Common.Grid (Grid, Position, Value, fromTuple, fromValue, toTuple, toValue, x, y)
import qualified Common.Grid as Grid (empty, insert, subgrid, toLines)
import Common.Tuple
import Debug.Trace (traceShow)

data PixelPosition = PixelPosition Int Int deriving (Eq, Ord)

instance Position PixelPosition where
  x (PixelPosition x' _) = x'
  y (PixelPosition _ y') = y'
  toTuple (PixelPosition x' y') = (x', y')
  fromTuple (x', y') = PixelPosition x' y'

fromCycle :: Int -> PixelPosition
fromCycle n = fromTuple . flipTuple . (`divMod` 40) $ n

--fromCycle n = fromTuple (posX, posY `mod` 6)
--  where
--    (posY, posX) = (`divMod` 40) n

newtype PixelValue = PixelValue Bool deriving (Eq)

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
draw spritePosition Screen {pixels = grid, cycles = screenCycles} = Screen {pixels = nextGrid, cycles = screenCycles + 1}
  where
    position = fromCycle screenCycles
    ledPosition = (`mod` 40) screenCycles
    isLed =
      (spritePosition)
        `elem` [
                 --            ledPosition - 2,
                 ledPosition - 1,
                 ledPosition,
                 ledPosition + 1
                 --        ledPosition + 2
                 --        ledPosition + 3
               ]
    pixel = PixelValue isLed
    nextGrid = Grid.insert position pixel . traceShow ("screenCycles", screenCycles, "ledpostion", ledPosition, "isLed", isLed, "position", toTuple position) $ grid

toLines :: Screen -> String
toLines Screen {pixels = grid} = Grid.toLines . Grid.subgrid (fromTuple (0, 0)) (40, 6) $ grid
