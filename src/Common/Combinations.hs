module Common.Combinations (ofItsElements) where

ofItsElements :: [a] -> [(a, a)]
ofItsElements xs = [(snd x, snd y) | x <- indexed, y <- indexed, fst x /= fst y]
  where
    indices = [0 ..] :: [Int]
    indexed = zip indices xs
