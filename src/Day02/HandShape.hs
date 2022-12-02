module Day02.HandShape (HandShape (Rock, Paper, Scissors), readLeft, readRight, score) where

data HandShape = Rock | Paper | Scissors deriving (Show, Eq)

readLeft :: Char -> HandShape
readLeft 'A' = Rock
readLeft 'B' = Paper
readLeft 'C' = Scissors
readLeft c = error ("Unknown HandShape '" ++ [c] ++ "'")

readRight :: Char -> HandShape
readRight 'X' = Rock
readRight 'Y' = Paper
readRight 'Z' = Scissors
readRight c = error ("Unknown HandShape '" ++ [c] ++ "'")

score :: HandShape -> Int
score Rock = 1
score Paper = 2
score Scissors = 3
