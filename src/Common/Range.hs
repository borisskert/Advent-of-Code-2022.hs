module Common.Range (Range, gap, from, union, size) where

import Common.Maybe
import Data.Range ((+=+))
import qualified Data.Range as R

data Range a = Range [R.Range a] deriving (Eq, Show)

from :: Ord a => a -> a -> Range a
from a b
  | a <= b = Range [a +=+ b]
  | otherwise = Range [b +=+ a]

union :: (Integral a, Show a) => Range a -> Range a -> Range a
union (Range other) (Range myRange) = Range (R.union myRange other)

size :: (Integral a) => Range a -> a
size (Range ranges) = sum . map lengthOf $ ranges

lengthOf :: (Integral a) => R.Range a -> a
lengthOf (R.SingletonRange _) = 1
lengthOf (R.SpanRange start end) = (R.boundValue end) - (R.boundValue start + 1)
lengthOf (R.LowerBoundRange _) = error "lengthOf LowerBoundRange is infinite"
lengthOf (R.UpperBoundRange _) = error "lengthOf UpperBoundRange is infinite"
lengthOf (R.InfiniteRange) = error "lengthOf InfiniteRange is infinite"

gap :: (Integral a, Show a) => Range a -> Maybe a
gap (Range ranges)
  | length ranges == 1 = Nothing
  | otherwise = Just . (+ 1) . endOf . head $ ranges

endOf :: R.Range a -> a
endOf (R.SingletonRange a) = a
endOf (R.SpanRange _ end) = R.boundValue end
endOf (R.LowerBoundRange _) = error "endOf LowerBoundRange is infinite"
endOf (R.UpperBoundRange end) = R.boundValue end
endOf (R.InfiniteRange) = error "endOf InfiniteRange is infinite"
