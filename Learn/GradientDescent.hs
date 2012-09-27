{-# LANGUAGE BangPatterns #-}

module Learn.GradientDescent where

import Data.Array.Repa as R

import Learn.Types
import Learn.NN
import Learn.Optimization

step :: Monad m => NN -> UMat -> UMat -> Double -> m (NN, Double)
step nn x y a = do
  (c, g) <- costNGradient nn x y
  return (nnSumS nn g (negate a), c)

runGradientDescent :: Monad m => NN -> UMat -> UMat -> Double -> Int -> m (NN, Double)
runGradientDescent nn x y alpha iter = do
  r@(nn', _) <- step nn x y alpha
  if iter < 2 then return r else runGradientDescent nn' x y alpha (iter - 1)

gradientDescent :: Monad m => StopCondition -> Double -> Function m -> UVec -> m (UVec, Double)
gradientDescent sc alpha fn start =
  do
    (f₀, f₀') <- fn start
    loop start f₀ f₀' 0
  where
    loop x_ f_ f_' !i = do
      let x = computeS $ x_ -^ R.map (*alpha) f_'
      (f, f') <- fn x
      if checkIter sc i || checkTol sc f_ f
        then return (x, f)
        else loop x f f' (i + 1)
        
        