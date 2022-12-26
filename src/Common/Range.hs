module Common.Range (lengthOf, startOf, endOf, gaps) where

import Data.Range

lengthOf :: (Integral a) => Range a -> a
lengthOf (SingletonRange _) = 1
lengthOf (SpanRange start end) = boundValue end - (boundValue start + 1)
lengthOf (LowerBoundRange _) = error "lengthOf LowerBoundRange is infinite"
lengthOf (UpperBoundRange _) = error "lengthOf UpperBoundRange is infinite"
lengthOf InfiniteRange = error "lengthOf InfiniteRange is infinite"

startOf :: Range a -> a
startOf (SingletonRange a) = a
startOf (SpanRange start _) = boundValue start
startOf (LowerBoundRange start) = boundValue start
startOf (UpperBoundRange _) = error "startOf LowerBoundRange is infinite"
startOf InfiniteRange = error "endOf InfiniteRange is infinite"

endOf :: Range a -> a
endOf (SingletonRange a) = a
endOf (SpanRange _ end) = boundValue end
endOf (LowerBoundRange _) = error "endOf LowerBoundRange is infinite"
endOf (UpperBoundRange end) = boundValue end
endOf InfiniteRange = error "endOf InfiniteRange is infinite"

gaps :: (Ord a) => [Range a] -> [Range a]
gaps = tail . init . invert
