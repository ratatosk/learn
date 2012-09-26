module Main where

import Control.Applicative

import System.Environment

import Data.Array.Repa as R

import Learn.Types
import Learn.Optimization
import Learn.ConjugateGradient

-- function that is just a sum of squares of all components shifted right by 1. Minimum is at (1,1,1,...1)
func :: Monad m => UVec -> m (Double, UVec)
func x = return (val, grad)
  where
    val = sumAllS $ R.map (\a -> (a - 1)*(a - 1)) x
    grad = computeS $ R.map (\a -> (2*a - 2)) x

main :: IO ()
main = do
  dim <- read . head <$> getArgs :: IO Int
  let start = fromListUnboxed (Z :. dim) $ replicate dim 0
      sc = StopCondition { tol = Nothing
                         , maxIter = Just 10 }
  res <- conjugateGradient sc func start
  print res