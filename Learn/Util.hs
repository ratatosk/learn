{-# LANGUAGE FlexibleContexts #-}

module Learn.Util where

import Data.Array.Repa as R
import Data.Array.Repa.Repr.Unboxed as R

classesToPredictions :: (Integral e, Source r e) => Int -> Array r DIM1 e -> Array U DIM2 Double
classesToPredictions nClasses c = computeS 
                                  $ fromFunction sh 
                                  $ \(Z :. row :. col) -> if col == fromIntegral (c ! (Z :. row)) then 1.0 else 0.0
  where
    (Z :. m) = extent c
    sh = (Z :. m :. nClasses)
         
maxIdx :: (Source r Double) => Array r DIM1 Double -> Int
maxIdx a = maxIdx' 0 0
  where
    (Z :. n) = extent a
    maxIdx' cur ptr | ptr == n = cur
                    | otherwise = if a ! (Z :. ptr) > a ! (Z :. cur)
                                  then maxIdx' ptr (ptr + 1)
                                  else maxIdx' cur (ptr + 1)
           
predictionsToClasses :: (Integral e, Unbox e, Source r e, Source r Double) => Array r DIM2 Double -> Array U DIM1 e
predictionsToClasses c = computeS 
                         $ fromFunction (Z :. r) 
                         $ \(Z :. i) -> fromIntegral $ maxIdx $ slice c (Any :. i :. All)
  where
    (Z :. r :. _) = extent c

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (a, b) = (a, f b)