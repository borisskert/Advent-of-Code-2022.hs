module Day09.Move (Direction, Move, up, down, left, right, readMany, go) where

import Common.Split

type Steps = Int

data Direction = R | U | L | D deriving (Eq, Show)

data Move = Move Direction Steps deriving (Eq, Show)

up :: Steps -> Move
up = Move U

down :: Steps -> Move
down = Move D

left :: Steps -> Move
left = Move L

right :: Steps -> Move
right = Move R

readOne :: String -> Move
readOne s = create direction
  where
    (direction, stepsPart) = splitPairOn " " s
    steps = read stepsPart

    create "R" = right steps
    create "L" = left steps
    create "U" = up steps
    create "D" = down steps
    create c = error ("Cannot read from '" ++ c ++ "'")

readMany :: String -> [Move]
readMany = map readOne . lines

type Position = (Int, Int)

go :: Move -> Position -> [Position]
go (Move U steps) (x, y) = map (\s -> (x, y + s)) [1 .. steps]
go (Move D steps) (x, y) = map (\s -> (x, y - s)) [1 .. steps]
go (Move R steps) (x, y) = map (\s -> (x + s, y)) [1 .. steps]
go (Move L steps) (x, y) = map (\s -> (x - s, y)) [1 .. steps]
