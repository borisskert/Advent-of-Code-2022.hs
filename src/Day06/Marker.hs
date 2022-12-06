module Day06.Marker (Marker, size, isValid, findIn) where

import Common.List

newtype Marker = Marker String deriving (Show, Eq)

size :: Marker -> Int
size (Marker xs) = length xs

isValid :: Marker -> Bool
isValid (Marker xs) = isNub . lastN 4 $ xs

findIn :: String -> Maybe Marker
findIn xs = safeHead . filter isValid . map (Marker . (`take` xs)) $ [4 .. (length xs)]
