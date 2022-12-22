module Day15.Device (Device, beacon, sensor, noDevice) where

import Common.Grid (Value, fromValue, toValue)

data Device = Beacon | Sensor | NoDevice deriving (Eq, Show, Ord)

beacon :: Device
beacon = Beacon

sensor :: Device
sensor = Sensor

noDevice :: Device
noDevice = NoDevice

instance Value Device where
  fromValue (Just Beacon) = 'B'
  fromValue (Just Sensor) = 'S'
  fromValue (Just NoDevice) = '#'
  fromValue Nothing = '.'
  toValue = error "Day15.Material.toValue: not implemented"
