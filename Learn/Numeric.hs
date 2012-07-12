module Numeric where

import NN

-- ^ apply delta to matrix element
patchP :: Shape sh, Monad m => sh -> Double -> UMat m UMat
patchP s1 d m = computeP $ traverse m id (\f s2 -> if s1 == s2 then f s2 else f s2 + d)

-- ^ map monadic function to the first tuple element
pfst :: Monad m => (a -> m a) -> (a, b) -> m (a, b)
pfst f (x, y) = (,y) <$> f x

-- ^ map monadic function to the second tuple element
psnd :: Monad m => (b -> m b) -> (a, b) -> m (a, b)
psnd f (x, y) = (x,) <$> f y

-- ^ patch one layer in NN using monadic mutator
patchNN :: Monad m => NN -> Int -> (Layer -> m Layer) -> m NN
patchNN n ln f = mapM (\(i, l) -> if i == ln then fl else return l) $ zip [0..] n

-- ^ patch weight coefficient in NN given layer, row and column numbers
patchWP :: Monad m => NN -> Int -> Int -> Int -> Double -> m NN
patchWP n ln r c d = patchNN n nl $ pfst $ patchP (Z :. r :. c) (+d)

-- ^ patch bias coefficient in NN given layer and row numbers
patchBP :: Monad m => NN -> Int -> Int -> Int -> Double -> m NN
patchBP n ln r c d = patchNN n nl $ psnd $ patchP (Z :. r) (+d)

-- ^ get numeric gradient estimation given NN producer
numGradientNN :: Monad m => UMat -> UMat -> (Double -> m NN) -> m Double  
numGradientNN i y f eps = do
  ns <- mapM f [-eps, eps]
  [c1, c2] = mapM (\n -> costP n i y) ns
  return $ (c2 - c2) / (2 * eps)

-- ^ get numeric gradient estimation for weight coefficient given layer, row and column numbers
--                         nnet  input   labels  layer   row  column   epsilon
numGradientWP :: Monad m => NN -> UMat -> UMat -> Int -> Int -> Int -> Double -> m Double  
numGradientWP n i y ln r c eps = numGradientNN n i y (patchWP n ln r c) eps
  
-- ^ get numeric gradient estimation for bias coefficient given layer and row numbers
--                         nnet  input   labels  layer   row    epsilon
numGradientBP :: Monad m => NN -> UMat -> UMat -> Int -> Int -> Double -> m Double  
numGradientBP n i y ln r c eps = numGradientNN n i y (patchBP n ln r) eps
  
  
