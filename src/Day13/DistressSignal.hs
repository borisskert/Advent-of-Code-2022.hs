module Day13.DistressSignal (indicesSum, decoderKey) where

import Common.List
import Data.List (sort)
import Day13.Signal (dividers, isDivider)
import Day13.SignalPair (hasCorrectOrder)
import qualified Day13.SignalPair as SignalPair (toList)
import Day13.SignalPairs (toList)

indicesSum :: String -> Int
indicesSum = sum . map ((+ 1) . fst) . filter (hasCorrectOrder . snd) . zipWithIndex . toList . read

decoderKey :: String -> Int
decoderKey =
  product
    . map ((+ 1) . fst)
    . filter (isDivider . snd)
    . zipWithIndex
    . sort
    . (dividers ++)
    . concatMap SignalPair.toList
    . toList
    . read
