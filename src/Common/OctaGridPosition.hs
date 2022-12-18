module Common.OctaGridPosition
  ( Position,
    adjacent,
    areAdjacent,
    stepsNorth,
    stepsSouth,
    stepsWest,
    stepsEast,
  )
where

import qualified Common.Grid as Grid (Position, fromTuple, x, y)

data Position = Position Int Int deriving (Eq, Show, Ord)

instance Grid.Position Position where
  x (Position myX _) = myX
  y (Position _ myY) = myY
  fromTuple (myX, myY) = Position myX myY

adjacent :: Position -> [Position]
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
areAdjacent pos other = other `elem` adjacent pos

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
