module Main where

import Control.Monad
import Control.Applicative

import Data.Array.Repa as R hiding ((++))

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

-- FIXME: cost calculates to NaN on the first step if we use 1000 hidden units. Investigation needed.

main :: IO ()
main = do
  [trainI, trainL, testI, testL, itersS] <- getArgs

  let iters = read itersS

  xTrain <- computeS . R.map fromIntegral <$> readLinearImages trainI
  yTrain <- classesToPredictions 10 <$> readLabels trainL
  let (Z :. trainImgNum :. imgSize) = extent xTrain
      (Z :. trainLabelNum :. _) = extent yTrain 
  when (trainLabelNum /= trainImgNum) $ error "Label/Image number mismatch in training data"

  putStrLn $ "Image size (pixels): " ++ show imgSize
  putStrLn $ "Training samples: " ++ show trainImgNum
    
  let shape = [imgSize, 500, 10]
      inn = randInit 123 shape
  putStrLn $ xTrain `deepSeqArray` yTrain `deepSeqArray` "Starting learning..."

  let func = ts2Function shape xTrain yTrain
      sc = StopCondition { tol = Nothing
                         , maxIter = Just iters }
      start = nn2Vector inn

  (nnv, c) <- conjugateGradient sc func start
  
  let nn = vector2NN shape nnv

  xTest <- computeS . R.map fromIntegral <$> readLinearImages testI
  yTestLabels <- readLabels testL
  let (Z :. testImgNum :. testImgSize) = extent xTest
      (Z :. testLabelNum) = extent yTestLabels
  when (testLabelNum /= testImgNum) $ error "Label/Image number mismatch in testing data"
  when (testImgSize /= imgSize) $ error "Train/Test image size mismatch"
  
  ans <- predictionsToClasses <$> hypothesis nn xTest

  let correct = length $ filter id $ Prelude.zipWith (==) (toList yTestLabels) (toList ans)
      
  print c
  print correct
  
  
