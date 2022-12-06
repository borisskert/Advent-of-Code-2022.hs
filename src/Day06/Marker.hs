module Day06.Marker (Marker, size, isValid, findIn) where

import Common.List

newtype Marker = Marker String deriving (Show, Eq)

size :: Marker -> Int
size (Marker xs) = length xs

isValid :: Int -> Marker -> Bool
isValid distinct (Marker xs) = isNub . lastN distinct $ xs

findIn :: Int -> String -> Maybe Marker
findIn distinct xs =
  safeHead
    . filter (isValid distinct)
    . map (Marker . (`take` xs))
    $ [distinct .. (length xs)]
