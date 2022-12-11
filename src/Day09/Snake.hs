module Day09.Snake (Snake, create, perform, visited, moveTail) where

import Common.OctaGrid (OctaGrid, Position)
import qualified Common.OctaGrid as OctaGrid (alter, areAdjacent, empty, insert, toList)
import Data.Maybe (fromMaybe)
import Day09.Move (Move)
import qualified Day09.Move as Move

data Snake = Snake Int (OctaGrid Int) [Position]

create :: Int -> Snake
create maxLength = Snake maxLength grid positions
  where
    grid = OctaGrid.insert (0, 0) 1 OctaGrid.empty
    positions = map (const (0, 0)) $ [0 .. maxLength]

perform :: Move -> Snake -> Snake
perform move snake@(Snake maxLength grid positions) = foldl (flip moveTo) snake $ nextHeadPositions
  where
    headPosition = headOf snake
    nextHeadPositions = Move.go move headPosition

moveTo :: Position -> Snake -> Snake
moveTo nextHeadPos (Snake maxLength grid positions)
  | tailPosition == nextTailPos = Snake maxLength grid nextPositions
  | otherwise = Snake maxLength nextGrid nextPositions
  where
    tailPosition = last positions
    nextPositions = moveTail nextHeadPos positions
    nextTailPos = last nextPositions
    nextGrid = OctaGrid.alter (Just . (+ 1) . fromMaybe 0) nextTailPos grid

moveTail :: Position -> [Position] -> [Position]
moveTail _ [] = []
moveTail nextLast [_] = [nextLast]
moveTail nextHeadPosition@(nextHeadX, nextHeadY) (_ : other@(otherX, otherY) : xs) = (nextHeadX, nextHeadY) : moveTail (nextOtherX, nextOtherY) ((otherX, otherY) : xs)
  where
    diffOtherX = nextHeadX - otherX
    diffOtherY = nextHeadY - otherY

    nextOtherX
      | OctaGrid.areAdjacent nextHeadPosition other = otherX
      | otherwise = otherX + signum diffOtherX
    nextOtherY
      | OctaGrid.areAdjacent nextHeadPosition other = otherY
      | otherwise = otherY + signum diffOtherY

headOf :: Snake -> Position
headOf (Snake _ _ positions) = head positions

visited :: Snake -> [Position]
visited (Snake _ grid _) = map fst . filter ((> 0) . snd) . OctaGrid.toList $ grid
