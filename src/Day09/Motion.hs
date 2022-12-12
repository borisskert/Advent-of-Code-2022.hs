module Day09.Motion (Direction, Motion, up, down, left, right, readMany, go) where

import Common.Split

type Steps = Int

data Direction = R | U | L | D deriving (Eq, Show)

data Motion = Motion Direction Steps deriving (Eq, Show)

up :: Steps -> Motion
up = Motion U

down :: Steps -> Motion
down = Motion D

left :: Steps -> Motion
left = Motion L

right :: Steps -> Motion
right = Motion R

readOne :: String -> Motion
readOne s = create direction
  where
    (direction, stepsPart) = splitPairOn " " s
    steps = read stepsPart

    create "R" = right steps
    create "L" = left steps
    create "U" = up steps
    create "D" = down steps
    create c = error ("Cannot read from '" ++ c ++ "'")

readMany :: String -> [Motion]
readMany = map readOne . lines

type Position = (Int, Int)

go :: Motion -> Position -> [Position]
go (Motion U steps) (x, y) = map (\s -> (x, y + s)) [1 .. steps]
go (Motion D steps) (x, y) = map (\s -> (x, y - s)) [1 .. steps]
go (Motion R steps) (x, y) = map (\s -> (x + s, y)) [1 .. steps]
go (Motion L steps) (x, y) = map (\s -> (x - s, y)) [1 .. steps]
