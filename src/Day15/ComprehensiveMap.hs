module Day15.ComprehensiveMap (ComprehensiveMap) where

import Common.Grid (Grid)
import Common.OctaGridPosition (Position)
import Day15.Device

newtype ComprehensiveMap = ComprehensiveMap (Grid Position Device) deriving (Eq, Show)
