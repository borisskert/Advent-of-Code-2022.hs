module Day09.Field (Field, empty, perform, visited) where

import Common.OctaGrid (OctaGrid, Position)
import qualified Common.OctaGrid as OctaGrid (alter, areAdjacent, empty, insert, toList)
import Data.Maybe (fromMaybe)
import Day09.Move (Move)
import qualified Day09.Move as Move (go)

--                                headPosition tailPosition
data Field = Field (OctaGrid Int) Position Position

empty :: Field
empty = Field (OctaGrid.insert (0, 0) 1 OctaGrid.empty) (0, 0) (0, 0)

perform :: Move -> Field -> Field
perform move field@(Field _ headPosition _) = foldl (flip moveTo) field nextHeadPos
  where
    nextHeadPos = Move.go move headPosition

moveTo :: Position -> Field -> Field
moveTo nextHeadPos (Field grid headPosition tailPosition)
  | tailPosition == nextTailPos = Field grid nextHeadPos nextTailPos
  | otherwise = Field nextGrid nextHeadPos nextTailPos
  where
    nextTailPos = moveTail headPosition nextHeadPos tailPosition
    nextGrid = OctaGrid.alter (Just . (+ 1) . fromMaybe 0) nextTailPos grid

moveTail :: Position -> Position -> Position -> Position
moveTail headPosition nextHeadPosition tailPosition
  | headPosition == tailPosition = tailPosition
  | nextHeadPosition == tailPosition = tailPosition
  | OctaGrid.areAdjacent nextHeadPosition tailPosition = tailPosition
  | otherwise = headPosition

visited :: Field -> [Position]
visited (Field grid _ _) = map fst . filter ((> 0) . snd) . OctaGrid.toList $ grid
