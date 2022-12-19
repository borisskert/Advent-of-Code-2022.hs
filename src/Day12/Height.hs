module Day12.Height (Height, fromChar, start, end, from, arePassable, elevation) where

import qualified Common.Grid as Grid (Value, fromValue, toValue)
import Data.Char (chr, ord)

data Height = Height Char | Start | End deriving (Eq, Show, Ord)

instance Grid.Value Height where
  toValue (_, c) = Just . fromChar $ c
  fromValue (Just Start) = 'S'
  fromValue (Just End) = 'E'
  fromValue (Just (Height c)) = c
  fromValue Nothing = '_'

start :: Height
start = Start

end :: Height
end = End

offset :: Int
offset = ord 'a'

from :: Int -> Height
from (-1) = Start
from 27 = End
from i = Height . chr . (+ offset) $ i

fromChar :: Char -> Height
fromChar 'S' = Start
fromChar 'E' = End
fromChar c = Height c

elevation :: Height -> Int
elevation Start = 0
elevation End = 25
elevation (Height c) = subtract offset . ord $ c

arePassable :: Height -> Height -> Bool
arePassable a b = a >= b || abs (elevation a - elevation b) <= 1
