module Day02.RoundResult (RoundResult (Win, Lose, Draw), readOne) where

data RoundResult = Win | Lose | Draw

readOne :: Char -> RoundResult
readOne 'X' = Lose
readOne 'Y' = Draw
readOne 'Z' = Win
readOne c = error ("Unknown RoundResult '" ++ [c] ++ "'")
