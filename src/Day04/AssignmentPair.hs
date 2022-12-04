module Day04.AssignmentPair (AssignmentPair, pairOf, readOne, readMany, isFullyContained, isOverlapped) where

import Common.Split (splitPairOn)
import Day04.Assignment (Assignment, overlaps)
import qualified Day04.Assignment as Assignment (contains, readOne)

data AssignmentPair = AssignmentPair Assignment Assignment deriving (Show, Eq)

pairOf :: Assignment -> Assignment -> AssignmentPair
pairOf = AssignmentPair

readOne :: String -> AssignmentPair
readOne s = AssignmentPair (Assignment.readOne left) (Assignment.readOne right)
  where
    (left, right) = splitPairOn "," s

readMany :: String -> [AssignmentPair]
readMany = map readOne . lines

isFullyContained :: AssignmentPair -> Bool
isFullyContained (AssignmentPair a b) = a `Assignment.contains` b || b `Assignment.contains` a

isOverlapped :: AssignmentPair -> Bool
isOverlapped (AssignmentPair a b) = a `overlaps` b
