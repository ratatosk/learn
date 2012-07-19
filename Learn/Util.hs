{-# LANGUAGE FlexibleContexts #-}

module Learn.Util where

import Data.Array.Repa as R

classesToPredictions :: (Monad m, Source r Int) => Array r DIM1 Int -> Int -> m (Array U DIM2 Double)
classesToPredictions c nClasses = computeP 
                                  $ fromFunction sh 
                                  $ \(Z :. row :. col) -> if col == c ! (Z :. row) then 1.0 else 0.0
  where
    (Z :. m) = extent c
    sh = (Z :. m :. nClasses)
