module Common.OctaGridPosition
  ( Position,
    from,
    areAdjacent,
    north,
    south,
    west,
    east,
    stepsNorth,
    stepsSouth,
    stepsWest,
    stepsEast,
    manhattanDistance,
    withinManhattanDistance,
  )
where

import qualified Common.Grid as Grid (Position, adjacent, fromTuple, x, y)
import Data.Set (Set)
import qualified Data.Set as Set (fromList)

data Position = Position Int Int deriving (Eq, Show, Ord)

from :: Int -> Int -> Position
from = Position

instance Grid.Position Position where
  x (Position myX _) = myX
  y (Position _ myY) = myY
  fromTuple (myX, myY) = Position myX myY
  adjacent (Position myX myY) =
    [ Position (myX - 1) (myY - 1),
      Position myX (myY - 1),
      Position (myX + 1) (myY - 1),
      Position (myX - 1) myY,
      Position (myX + 1) myY,
      Position (myX - 1) (myY + 1),
      Position myX (myY + 1),
      Position (myX + 1) (myY + 1)
    ]

areAdjacent :: Position -> Position -> Bool
areAdjacent pos other = other `elem` Grid.adjacent pos

north :: Position -> Position
north (Position myX myY) = Position myX (myY + 1)

south :: Position -> Position
south (Position myX myY) = Position myX (myY - 1)

west :: Position -> Position
west (Position myX myY) = Position (myX - 1) myY

east :: Position -> Position
east (Position myX myY) = Position (myX + 1) myY

stepsNorth :: Int -> Position -> [Position]
stepsNorth steps pos = scanl (\p _ -> north p) pos [1 .. steps]

stepsSouth :: Int -> Position -> [Position]
stepsSouth steps pos = scanl (\p _ -> south p) pos [1 .. steps]

stepsEast :: Int -> Position -> [Position]
stepsEast steps pos = scanl (\p _ -> east p) pos [1 .. steps]

stepsWest :: Int -> Position -> [Position]
stepsWest steps pos = scanl (\p _ -> west p) pos [1 .. steps]

manhattanDistance :: Position -> Position -> Int
manhattanDistance (Position x y) (Position x' y') = abs (x - x') + abs (y - y')

withinManhattanDistance :: Int -> Position -> Set Position
withinManhattanDistance m pos = Set.fromList . concatMap columnAt $ xs
  where
    myX = Grid.x pos
    myY = Grid.y pos
    xs = [(myX - m) .. (myX + m)]

    columnAt :: Int -> [Position]
    columnAt x = (from x myY :) . concatMap (\y -> [from x (negate y + myY), from x (y + myY)]) $ [1 .. size]
      where
        size = m - abs (myX - x)
