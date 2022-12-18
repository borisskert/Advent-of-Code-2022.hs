module Day12.Height (Height, start, end, from, arePassable) where

import qualified Common.Grid as Grid (Value, fromValue, toValue)
import Data.Char (chr, ord)

data Height = Height Int | Start | End deriving (Eq, Show, Ord)

offset :: Int
offset = ord 'a'

instance Grid.Value Height where
  toValue (_, 'S') = Just Start
  toValue (_, 'E') = Just End
  toValue (_, c) = Just . Height . subtract offset . ord $ c
  fromValue (Just Start) = 'S'
  fromValue (Just End) = 'E'
  fromValue (Just (Height i)) = chr . (+ offset) $ i
  fromValue Nothing = '_'

start :: Height
start = Start

end :: Height
end = End

from :: Int -> Height
from = Height

elevation :: Height -> Int
elevation (Height h) = h
elevation Start = subtract offset . ord $ 'a'
elevation End = subtract offset . ord $ 'z'

arePassable :: Height -> Height -> Bool
arePassable a b = abs (elevation a - elevation b) <= 1
