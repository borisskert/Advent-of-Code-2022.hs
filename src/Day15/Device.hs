module Day15.Device (Device) where

import Common.Grid (Value, fromValue, toValue)

data Device = Beacon | Sensor deriving (Eq, Show)

beacon :: Device
beacon = Beacon

sensor :: Device
sensor = Sensor

instance Value Device where
  fromValue (Just Beacon) = 'B'
  fromValue (Just Sensor) = 'S'
  fromValue Nothing = '.'
  toValue = error "Day15.Material.toValue: not implemented"
