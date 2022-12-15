module Day10.CathodeRayTube (signalStrengthSum, screenOutput) where

import Day10.Device

signalStrengthSum :: String -> Int
signalStrengthSum input = sum . map (signalStrength . (`ticks` device)) $ [20, 60, 100, 140, 180, 220]
  where
    program = read input
    device = load program crtDevice

screenOutput :: String -> String
screenOutput input = show . run $ device
  where
    program = read input
    device = load program crtDevice
