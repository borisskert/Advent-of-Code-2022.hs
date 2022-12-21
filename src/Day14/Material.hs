module Day14.Material (Material, rock, sand, sandSource) where

import Common.Grid (Value, fromValue, toValue)

-- | -------------------------------------------------------------------------------------------------------------------
-- | Material data structure
-- | -------------------------------------------------------------------------------------------------------------------
data Material = Sand | Rock | SandSource deriving (Eq, Show, Ord)

instance Value Material where
  toValue = undefined
  fromValue (Just Sand) = 'o'
  fromValue (Just Rock) = '#'
  fromValue (Just SandSource) = '+'
  fromValue Nothing = '.'

rock :: Material
rock = Rock

sandSource :: Material
sandSource = SandSource

sand :: Material
sand = Sand
