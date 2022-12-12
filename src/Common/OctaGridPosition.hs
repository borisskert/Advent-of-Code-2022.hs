module Common.OctaGridPosition
  ( Position,
    x,
    y,
    fromTuple,
    toTuple,
    adjacent,
    areAdjacent,
    north,
    south,
    west,
    east,
    stepsNorth,
    stepsSouth,
    stepsWest,
    stepsEast,
  )
where

data Position = Position {x :: Int, y :: Int} deriving (Eq, Show, Ord)

fromTuple :: (Int, Int) -> Position
fromTuple (myX, myY) = Position {x = myX, y = myY}

toTuple :: Position -> (Int, Int)
toTuple Position {x = myX, y = myY} = (myX, myY)

adjacent :: Position -> [Position]
adjacent Position {x = myX, y = myY} =
  [ Position {x = myX - 1, y = myY - 1},
    Position {x = myX, y = myY - 1},
    Position {x = myX + 1, y = myY - 1},
    Position {x = myX -1, y = myY},
    Position {x = myX + 1, y = myY},
    Position {x = myX - 1, y = myY + 1},
    Position {x = myX, y = myY + 1},
    Position {x = myX + 1, y = myY + 1}
  ]

areAdjacent :: Position -> Position -> Bool
areAdjacent pos other = other `elem` adjacent pos

north :: Position -> Position
north Position {x = myX, y = myY} = Position {x = myX, y = myY + 1}

south :: Position -> Position
south Position {x = myX, y = myY} = Position {x = myX, y = myY - 1}

west :: Position -> Position
west Position {x = myX, y = myY} = Position {x = myX -1, y = myY}

east :: Position -> Position
east Position {x = myX, y = myY} = Position {x = myX + 1, y = myY}

stepsNorth :: Int -> Position -> [Position]
stepsNorth steps pos = scanl (\p _ -> north p) pos [1 .. steps]

stepsSouth :: Int -> Position -> [Position]
stepsSouth steps pos = scanl (\p _ -> south p) pos [1 .. steps]

stepsEast :: Int -> Position -> [Position]
stepsEast steps pos = scanl (\p _ -> east p) pos [1 .. steps]

stepsWest :: Int -> Position -> [Position]
stepsWest steps pos = scanl (\p _ -> west p) pos [1 .. steps]
