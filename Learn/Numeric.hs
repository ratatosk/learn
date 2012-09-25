{-# LANGUAGE TupleSections #-}
module Learn.Numeric where

import Control.Monad (liftM)

import Data.Array.Repa as R

import Learn.Types
import Learn.NN

-- ^ apply delta to matrix element
patchP :: (Shape sh, Monad m) => sh -> Double -> R.Array U sh Double-> m (R.Array U sh Double)
patchP s1 d m = computeP $ traverse m id (\f s2 -> if s1 == s2 then f s2 + d else f s2)

-- ^ map monadic function to the first tuple element
pfst :: Monad m => (a -> m a) -> (a, b) -> m (a, b)
pfst f (x, y) = (,y) `liftM` f x

-- ^ map monadic function to the second tuple element
psnd :: Monad m => (b -> m b) -> (a, b) -> m (a, b)
psnd f (x, y) = (x,) `liftM` f y

-- ^ patch one layer in NN using monadic mutator
patchNN :: Monad m => NN -> Int -> (Layer -> m Layer) -> m NN
patchNN n ln f = mapM (\(i, l) -> if i == ln then f l else return l) $ zip [0..] n

-- ^ patch weight coefficient in NN given layer, row and column numbers
patchWP :: Monad m => NN -> Int -> Int -> Int -> Double -> m NN
patchWP n ln r c d = patchNN n ln $ pfst $ patchP (Z :. r :. c) d

-- ^ patch bias coefficient in NN given layer and row numbers
patchBP :: Monad m => NN -> Int -> Int -> Double -> m NN
patchBP n ln r d = patchNN n ln $ psnd $ patchP (Z :. r) d

-- TODO: make sure that gradient is not calculated (not sure about laziness)
-- ^ get numeric gradient estimation given NN producer
numGradientNN :: Monad m => UMat -> UMat -> (Double -> m NN) -> Double -> m Double  
numGradientNN i y f eps = do
  ns <- mapM f [-eps, eps]
  [c1, c2] <- mapM (\n -> liftM fst $ costNGradient n i y) ns
  return $ (c2 - c1) / (2 * eps)

-- ^ get numeric gradient estimation for weight coefficient given layer, row and column numbers
--                         nnet  input   labels  layer   row  column   epsilon
numGradientWP :: Monad m => NN -> UMat -> UMat -> Int -> Int -> Int -> Double -> m Double  
numGradientWP n i y ln r c = numGradientNN i y (patchWP n ln r c)
  
-- ^ get numeric gradient estimation for bias coefficient given layer and row numbers
--                         nnet  input   labels  layer   row    epsilon
numGradientBP :: Monad m => NN -> UMat -> UMat -> Int -> Int -> Double -> m Double  
numGradientBP n i y ln r = numGradientNN i y (patchBP n ln r)
