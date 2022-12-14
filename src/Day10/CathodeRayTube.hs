module Day10.CathodeRayTube (signalStrengthSum) where

import Day10.CPU

signalStrengthSum :: String -> Int
signalStrengthSum input = sum . map (signalStrength . (`ticks` cpu)) $ [20, 60, 100, 140, 180, 220]
  where
    program = read input
    cpu = load program simple
