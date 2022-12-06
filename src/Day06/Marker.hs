module Day06.Marker (Marker, size, isValid, findIn) where

import Common.List

newtype Marker = Marker String deriving (Show, Eq)

size :: Marker -> Int
size (Marker xs) = length xs

isValid :: Int -> Marker -> Bool
isValid n (Marker xs) = isDistinct . lastN n $ xs

findIn :: Int -> String -> Maybe Marker
findIn n xs =
  safeHead
    . filter (isValid n)
    . map (Marker . (`take` xs))
    $ [n .. (length xs)]
