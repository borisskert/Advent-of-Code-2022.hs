module Day14.Reservoir (Reservoir) where

import Common.Grid (Grid, Value)
import qualified Common.Grid as Grid (toValue, fromValue)
import Common.OctaGridPosition (Position)

data Material = Sand | Rock deriving (Eq, Show)

instance Value Material where
  toValue = undefined
  fromValue (Just Sand) = 'o'
  fromValue (Just Rock) = '#'
  fromValue Nothing = '.'

newtype Reservoir = Reservoir (Grid Position Material) deriving (Show, Eq)
