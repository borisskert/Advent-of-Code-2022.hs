module Common.Vector2D
  ( Vector2D,
    x,
    y,
    north,
    south,
    west,
    east,
    toTuple,
    fromTuple,
  )
where

class Vector2D a where
  x :: a -> Int
  y :: a -> Int
  north :: a -> a
  south :: a -> a
  west :: a -> a
  east :: a -> a
  toTuple :: a -> (Int, Int)
  fromTuple :: (Int, Int) -> a
