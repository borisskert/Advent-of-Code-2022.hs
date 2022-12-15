module Day10.CathodeRayTube (signalStrengthSum, crtOutput) where

import Day10.Device

signalStrengthSum :: String -> Int
signalStrengthSum input = sum . map (signalStrength . (`ticks` device)) $ [20, 60, 100, 140, 180, 220]
  where
    program = read input
    device = load program crtDevice

crtOutput :: String -> String
crtOutput input = screenOutput . run $ device
  where
    program = read input
    device = load program crtDevice
