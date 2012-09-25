module Main where

import Control.Applicative

import Data.Array.Repa as R

import Learn.Types
import Learn.IO
import Learn.NN
import Learn.Util
import Learn.GradientDescent

import System.Environment

main :: IO ()
main = do
  iters <- read . head <$> getArgs
  x <- readMat "/tmp/X.txt" 5000 400 :: IO UMat
  y <- readVec "/tmp/y.txt" 5000 :: IO (Array U DIM1 Int)
  y' <- classesToPredictions (R.map (\i -> if i == 10 then 0 else i) y) 10
  let inn = randInit 1 [400, 25, 10]
  putStrLn $ x `deepSeqArray` y' `deepSeqArray` "Starting gradient descent..."
  (nn, c) <- runGradientDescent inn x y' 1 iters
  h <- hypothesis nn x
  ans <- predictionsToClasses h
  let correct = length $ filter id $ Prelude.zipWith (==) (toList y) (toList ans)
  print c
  print correct
  
  
  