module Day13.DistressSignal (indicesSum) where

import Common.List
import Day13.SignalPair (hasCorrectOrder)
import Day13.SignalPairs (toList)

indicesSum :: String -> Int
indicesSum = sum . map ((+ 1) . fst) . filter (hasCorrectOrder . snd) . zipWithIndex . toList . read
