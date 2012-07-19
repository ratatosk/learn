module Learn.GradientDescent where

import Learn.NN

step :: Monad m => NN -> UMat -> UMat -> Double -> m (NN, Double)
step nn x y a = do
  (c, g) <- costNGradient nn x y
  return (nnSumS nn g a, c)

runGradientDescent :: Monad m => NN -> UMat -> UMat -> Double -> Int -> m (NN, Double)
runGradientDescent nn x y alpha iter = do
  r@(nn', _) <- step nn x y alpha
  if iter < 2 then return r else runGradientDescent nn' x y alpha (iter - 1)
  