module Common.Fold (times) where

times :: (a -> a) -> a -> Int -> a
times _ x 0 = x
times fn x n = times fn (fn x) (n - 1)
