module Day09.Rope (Rope, fromList, toList, moveTail, tag, headOf) where

import Common.OctaGrid (Position, areAdjacent)

type Knot = Position

newtype Rope = Rope [Knot] deriving (Eq, Show)

fromList :: [Knot] -> Rope
fromList = Rope

toList :: Rope -> [Knot]
toList (Rope knots) = knots

moveTail :: Knot -> Rope -> Rope
moveTail motion (Rope knots) = Rope . move motion $ knots

move :: Knot -> [Knot] -> [Knot]
move _ [] = []
move motion [_] = [motion]
move motion (_ : knot : xs) =
  (motionX, motionY) : move knotMotion (knot : xs)
  where
    (motionX, motionY) = motion
    (knotX, knotY) = knot

    diffX = motionX - knotX
    diffY = motionY - knotY

    knotMotionX
      | areAdjacent motion knot = knotX
      | otherwise = knotX + signum diffX
    knotMotionY
      | areAdjacent motion knot = knotY
      | otherwise = knotY + signum diffY

    knotMotion = (knotMotionX, knotMotionY)

tag :: Rope -> Knot
tag (Rope knots) = last knots

headOf :: Rope -> Knot
headOf (Rope knots) = head knots
