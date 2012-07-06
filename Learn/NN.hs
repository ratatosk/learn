module Learn.NN where

import Data.Array.Repa                  as R
--import Data.Array.Repa.Unsafe           as R

import Data.Array.Repa.Algorithms.Matrix
import Data.Array.Repa.Algorithms.Randomish

-- Number of neurons in every layer
type NNShape = [Int]

type UMat = Array U DIM2 Double
type UVec = Array U DIM1 Double

-- Input weights and bias unit weights
type Layer = (UMat, UVec)

-- Neural network - list of layers.
type NN = [Layer]

-- Here usual abbreviations are:
-- i - input
-- w - weight matrix
-- b - bias weights vector
-- y - labels (ground truth)

randInit :: Int -> NNShape -> NN
randInit _ [] = []
randInit _ [_] = []
randInit seed (f:s:ls) = (w, b) : randInit (seed + 2) (s:ls)
  where 
    w = randomishDoubleArray (Z :. s :. f) 0.0 0.1 seed
    b = randomishDoubleArray (Z :. s) 0.0 0.1 (seed + 1)

{-# INLINE sigmoid #-}
sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

{-# INLINE sigmoid' #-}
sigmoid' :: Double -> Double
sigmoid' x = sx * (1 - sx) where sx = sigmoid x
                                 
-- TODO: try to remove NOINLINE annotations
-- calculate weighted sums in parallel
-- TODO: switch to unsafeSlice after testing
{-# NOINLINE weightedSumsP #-}
weightedSumsP :: Monad m => UMat -> Layer -> m UMat
weightedSumsP i (w, b)
  = [i, w] `deepSeqArrays` b `deepSeqArray`
    do let (Z :. h1  :. _) = extent i
       let (Z :. w2  :. _) = extent w
       computeP 
         $ fromFunction (Z :. h1 :. w2)
         $ \ix -> b ! (Z :. col ix) + (R.sumAllS 
                                       $ R.zipWith (*)
                                       (slice i (Any :. (row ix) :. All))
                                       (slice w (Any :. (col ix) :. All)))
                  
-- TODO: Fuse it manually into mmultP to avoid allocation
-- A .* sigmoid'(B) (.* - elementWise multiplication)
{-# NOINLINE weightedSumsP #-}       
dotProdSigmP :: Monad m => UMat -> UMat -> UMat
dotProdSigmP a b = [a, b] `deepSeqArray` computeP
                  $ fromFunction (extent a)
                  $ \ix -> a ! ix * sigmoid' (b ! ix)
                  
{-# INLINE outError #-}
outError :: Double -> Double -> Double
outError z a y = (- (y - a) * sigmoid'(z)) 
                                 
-- returns lists of weighted sums and activations
forwardP :: Monad m => UMat -> NN -> m ([UMat], [UMat])
forwardP i l = liftM unzip $ forwardP' i l
  where
    forwardP' i [] = return []
    forwardP' i (l:ls) = do
      z <- weightedSumsP i l
      a <- computeP $ R.map sigmoid z
      nxt <- activationsP a ls
      return $ (z, a) : nxt
         
errorsP :: Monad m => UMat -> NN -> [UMat] -> [UMat] -> m [(UMat)]
errorsP y n z a = errors1 y (reverse n) (tail $ reverse z) (last a)
  where
    errors1 y' n' z' a' = do
      eOut <- computeP $ R.zipWith (-) y' 
      eHid <- errorsHid eOut n' z'
      return (eOut : eHid)
    
    errorsHid _ [] _ = []
    errorsHid nxt ((w:_):ls) z = 
do cur
    