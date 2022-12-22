module Day15.SignalRow (SignalRow, from, size, toList) where

import Common.OctaGridPosition (Position)
import Data.Set (Set)
import qualified Data.Set as Set (toList)

newtype SignalRow = SignalRow (Set Position) deriving (Eq, Show)

from :: Set Position -> SignalRow
from = SignalRow

size :: SignalRow -> Int
size (SignalRow mySet) = length mySet

toList :: SignalRow -> [Position]
toList (SignalRow mySet) = Set.toList mySet
