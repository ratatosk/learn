{-# LANGUAGE FlexibleContexts #-}

module Learn.Util where

import Data.Array.Repa as R

classesToPredictions :: (Source r Int) => Int -> Array r DIM1 Int -> Array U DIM2 Double
classesToPredictions nClasses c = computeS 
                                  $ fromFunction sh 
                                  $ \(Z :. row :. col) -> if col == c ! (Z :. row) then 1.0 else 0.0
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
           
predictionsToClasses :: (Source r Double) => Array r DIM2 Double -> Array U DIM1 Int
predictionsToClasses c = computeS 
                         $ fromFunction (Z :. r) 
                         $ \(Z :. i) -> maxIdx $ slice c (Any :. i :. All)
  where
    (Z :. r :. _) = extent c
