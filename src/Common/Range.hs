module Common.Range (Range, start, end, gap, from, excludeFrom, union, intersect, size, isOverlappingWith) where

import Common.Maybe
import Control.Monad (liftM2)
import Control.Monad.HT (liftJoin2)

data Range a = Range {start :: a, end :: a, gap :: Maybe (Range a)} deriving (Eq, Ord, Show)

from :: Ord a => a -> a -> Range a
from a b
  | a <= b = Range a b Nothing
  | otherwise = Range b a Nothing

excludeFrom :: (Integral a) => Range a -> Range a -> Maybe (Range a)
excludeFrom other@(Range otherStart otherEnd otherGaps) myRange@(Range myStart myEnd myGaps)
  | other == myRange = Nothing
  | myStart == otherStart && myEnd == otherEnd = liftJoin2 excludeFrom myGaps otherGaps
  | other `isLeadingIn` myRange = Just . maybeReplace excludeFrom myGaps . maybeDo union otherGaps . from (otherEnd + 1) $ myEnd
  | other `isTrailingIn` myRange = Just . maybeReplace excludeFrom myGaps . maybeDo union otherGaps . from myStart $ (otherStart - 1)
  | other `isWithin` myRange = Just . withGap unionGap . maybeDo union otherGaps $ from myStart myEnd
  | otherwise = Just . maybeReplace excludeFrom (other `intersect` myRange) $ myRange
  where
    unionGap = maybeDo union myGaps other

union :: (Integral a) => Range a -> Range a -> Range a
union other@(Range otherStart otherEnd otherGaps) myRange@(Range myStart myEnd myGaps)
  | other == myRange = myRange
  | otherStart == myStart && otherEnd == myEnd = Range myStart myEnd (liftM2 union otherGaps myGaps)
  | other `isWithin` myRange = Range myStart myEnd (liftM2 union otherGaps myGaps)
  | other `isOverlappingWith` myRange = Range (min otherStart myStart) (max otherEnd myEnd) (liftM2 union otherGaps myGaps)
  | other `isAdjacentOf` myRange = Range (min myStart otherStart) (max myEnd otherEnd) (liftM2 union otherGaps myGaps)
  | otherwise = replace (newGap `excludeFrom`) $ Range (min myStart otherStart) (max myEnd otherEnd) (liftM2 union otherGaps myGaps)
  where
    newGap = Range (min myEnd otherEnd + 1) (max myStart otherStart - 1) Nothing

intersect :: (Integral a) => Range a -> Range a -> Maybe (Range a)
intersect other@(Range otherStart otherEnd otherGaps) myRange@(Range myStart myEnd myGaps)
  | other == myRange = Just myRange
  | other `isWithin` myRange = maybeSkip excludeFrom myGaps other
  | myRange `isWithin` other = maybeSkip excludeFrom otherGaps myRange
  | newStart < newEnd = Just $ Range newStart newEnd (liftM2 union otherGaps myGaps)
  | otherwise = Nothing
  where
    newStart = max myStart otherStart
    newEnd = min myEnd otherEnd

size :: (Integral a) => Range a -> a
size Range {start = myStart, end = myEnd, gap = myGaps} =
  (myEnd - myStart) + 1 - gapSize
  where
    gapSize = maybe 0 size myGaps

isAdjacentOf :: (Ord a, Num a) => Range a -> Range a -> Bool
isAdjacentOf (Range otherStart otherEnd _) (Range myStart myEnd _) =
  myEnd + 1 == otherStart || otherEnd + 1 == myStart

isOverlappingWith :: Integral a => Range a -> Range a -> Bool
isOverlappingWith Range {start = otherStart, end = otherEnd} Range {start = myStart, end = myEnd} =
  myStart <= otherStart && otherStart <= myEnd || otherStart <= myStart && myStart <= otherEnd

isLeadingIn :: Integral a => Range a -> Range a -> Bool
isLeadingIn Range {start = otherStart, end = otherEnd} Range {start = myStart, end = myEnd} =
  otherStart == myStart && otherEnd < myEnd

isTrailingIn :: Integral a => Range a -> Range a -> Bool
isTrailingIn Range {start = otherStart, end = otherEnd} Range {start = myStart, end = myEnd} =
  otherEnd == myEnd && otherStart > myStart

isWithin :: Integral a => Range a -> Range a -> Bool
isWithin Range {start = otherStart, end = otherEnd} Range {start = myStart, end = myEnd} =
  otherStart >= myStart && otherEnd <= myEnd

withGap :: Range a -> Range a -> Range a
withGap newGap (Range myStart myEnd _) = Range myStart myEnd $ Just newGap
