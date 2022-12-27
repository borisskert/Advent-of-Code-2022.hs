module Day15.SignalRow (SignalRow, from, size, hole, union, withBeacon) where

import Common.OctaGridPosition (Position)
import qualified Common.OctaGridPosition as Position (from)
import Common.Range (lengthOf, startOf)
import qualified Common.Range as Range (gaps)
import Data.Range (Range, (+=+))
import qualified Data.Range as Range (joinRanges)
import Data.Set (Set)
import qualified Data.Set as Set (empty, insert, union)

-- | -------------------------------------------------------------------------------------------------------------------
-- | SignalRow is a row of the scanner area. It is a set of ranges.
-- | -------------------------------------------------------------------------------------------------------------------
data SignalRow = SignalRow {ranges :: [Range Int], rowY :: Int, beacons :: Set Int} deriving (Eq, Show)

from :: Int -> Int -> Int -> SignalRow
from start end row = SignalRow [start +=+ end] row Set.empty

withBeacon :: Int -> SignalRow -> SignalRow
withBeacon beaconX SignalRow {ranges = myRange, rowY = myRowY, beacons = myBeacons} =
  SignalRow myRange myRowY (Set.insert beaconX myBeacons)

size :: SignalRow -> Int
size (SignalRow myRanges _ myBeacons) = subtract (length myBeacons) . sum . map lengthOf $ myRanges

union :: SignalRow -> SignalRow -> SignalRow
union (SignalRow rangesA row beaconsA) (SignalRow rangesB _ beaconsB) =
  SignalRow mergedRanges row (Set.union beaconsA beaconsB)
  where
    mergedRanges = Range.joinRanges (rangesA ++ rangesB)

hole :: SignalRow -> Maybe Position
hole (SignalRow myRanges row _) = fmap (`Position.from` row) column
  where
    gaps = Range.gaps myRanges
    column
      | null gaps = Nothing
      | otherwise = Just . (+ 1) . startOf . head $ gaps
