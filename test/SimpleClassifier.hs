module Main where

import Control.Applicative

import Data.Array.Repa as R

import Data.Array.Repa.IO.Binary
import Data.Array.Repa.Repr.ForeignPtr

import Learn.Types
import Learn.IO
import Learn.NN
import Learn.Util
import Learn.GradientDescent
import Learn.Optimization
import Learn.ConjugateGradient

import System.Environment

{- /tmp/X.txt and /tmp/y.txt should be created like this:
in octave:
load("ex4data1.mat") % ex4data1.mat is from exercise4 of
                     % Stanford Machine Learning class (https://www.coursera.org/course/ml)
dlmwrite("/tmp/X.txt", X)
dlmwrite("/tmp/y.txt", y)

in shell:
sed -i 's/10/0/' /tmp/y.txt # in that assignment zeros are marked as 10th class
matconvert 5000 400 /tmp/X.txt
-}

main :: IO ()
main = do
  args <- getArgs
  let iters = read $ head args
      method = args !! 1
  -- x <- readMat "/tmp/X.txt" 5000 400 :: IO UMat
  x <- computeS . delay <$> readArrayFromStorableFile "/tmp/X.bin" (Z :. 5000 :. 400) :: IO (Array U DIM2 Double)
  y <- readVec "/tmp/y.txt" 5000 :: IO (Array U DIM1 Int)
  y' <- classesToPredictions y 10
  let shape = [400, 25, 10]
      inn = randInit 1 shape
  putStrLn $ x `deepSeqArray` y' `deepSeqArray` "Starting gradient descent..."

  let func = ts2Function shape x y'
      sc = StopCondition { tol = Nothing
                         , maxIter = Just iters }
      start = nn2Vector inn

  (nn, c) <- case method of
    "GD2" -> do
      (nnv, err) <- gradientDescent sc 1 func start
      return (vector2NN shape nnv, err)
    "GD" -> runGradientDescent inn x y' 20 iters
    "CG" -> do
      (nnv, err) <- conjugateGradient sc func start
      return (vector2NN shape nnv, err)
    _ -> error "method not supported"

  h <- hypothesis nn x
  ans <- predictionsToClasses h
  let correct = length $ filter id $ Prelude.zipWith (==) (toList y) (toList ans)
  print c
  print correct
  
  
