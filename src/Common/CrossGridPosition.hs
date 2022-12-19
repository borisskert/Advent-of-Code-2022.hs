module Common.CrossGridPosition (Position, from) where

import qualified Common.Grid as Grid (Position, adjacent, fromTuple, x, y)

data Position = Position Int Int deriving (Eq, Show, Ord)

from :: Int -> Int -> Position
from = Position

instance Grid.Position Position where
  x (Position myX _) = myX
  y (Position _ myY) = myY
  fromTuple (myX, myY) = Position myX myY
  adjacent (Position myX myY) =
    [ Position myX (myY - 1),
      Position (myX - 1) myY,
      Position (myX + 1) myY,
      Position myX (myY + 1)
    ]
