module Common.Either (rightOrThrow) where

rightOrThrow :: (Show a) => Either a b -> b
rightOrThrow (Right b) = b
rightOrThrow (Left a) = error . show $ a
