module Day02.HandShape (HandShape (Rock, Paper, Scissors), readLeft, readRight, score, with) where

import Day02.RoundResult (RoundResult (Draw, Lose, Win))

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

with :: HandShape -> RoundResult -> HandShape
with left Win = win left
with left Draw = left
with left Lose = lose left

lose :: HandShape -> HandShape
lose Rock = Scissors
lose Scissors = Paper
lose Paper = Rock

win :: HandShape -> HandShape
win Rock = Paper
win Scissors = Rock
win Paper = Scissors
