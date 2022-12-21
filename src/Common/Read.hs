module Common.Read (readBy) where

import Common.Either (rightOrThrow)

readBy :: (Show s) => (String -> Either s a) -> String -> [(a, String)]
readBy parse input = [(parsed, [])]
  where
    parsed = rightOrThrow . parse $ input
