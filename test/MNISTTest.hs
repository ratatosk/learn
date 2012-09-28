module Main where

import Control.Applicative

import Data.Array.Repa as R

import Learn.IO.MNIST
import Learn.NN
import Learn.Util
import Learn.Optimization
import Learn.ConjugateGradient

import System.Environment

{-
  MNIST Database can be downloaded from here:
  http://yann.lecun.com/exdb/mnist/

  4 files are needed:
  training set images
  training set labels
  test set images
  test set labels

  paths to all 4 files should be passed as command line parameters in the same order.
  fifth parameter should be number of iterations

  program assumes that training set contains 60,000 images and test set contains 10,000. (hardcoded below)
-}

trainSize, testSize :: Int
trainSize = 60000
testSize = 10000

main :: IO ()
main = do
  [trainI, trainL, testI, testL, itersS] <- getArgs
  let iters = read itersS

  xTrain <- readMat trainI trainSize 784 :: IO (Array U DIM2 Double)
  yTrain <- (classesToPredictions 10 <$> readVec trainL trainSize) :: IO (Array U DIM2 Double)
    
  let shape = [784, 25, 10]
      inn = randInit 1 shape
  putStrLn $ xTrain `deepSeqArray` yTrain `deepSeqArray` "Starting learning..."

  let func = ts2Function shape xTrain yTrain
      sc = StopCondition { tol = Nothing
                         , maxIter = Just iters }
      start = nn2Vector inn

  (nnv, c) <- conjugateGradient sc func start

  let nn = vector2NN shape nnv
  
  xTest <- readMat testI testSize 784 :: IO (Array U DIM2 Double)
  yTest <- readVec testL testSize :: IO (Array U DIM1 Int)

  ans <- predictionsToClasses <$> hypothesis nn xTest

  let correct = length $ filter id $ Prelude.zipWith (==) (toList yTest) (toList ans)
      
  print c
  print correct
  
  
