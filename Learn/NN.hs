module Learn.NN where

import Control.Monad (liftM)

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
-- x - input
-- w - weight matrix
-- b - bias weights vector
-- y - labels (ground truth)

-- global TODO: add regularization support
-- global TODO: make use of R.traverse*

randInit :: Int -> NNShape -> NN
randInit _ [] = []
randInit _ [_] = []
randInit seed (f:s:ls) = (w, b) : randInit (seed + 2) (s:ls)
  where 
    w = randomishDoubleArray (Z :. s :. f) 0.0 0.1 seed
    b = randomishDoubleArray (Z :. s) 0.0 0.1 (seed + 1)
    
randInput :: Int -> Int -> Int -> UMat
randInput seed h w = randomishDoubleArray (Z :. h :. w) 0.0 1.0 seed

yFromList :: [Double] -> Int -> UMat
yFromList ys nc = fromListUnboxed (Z :. (length ys `div` nc) :. nc) ys

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
    do let (Z :. h1  :. _) = extent i -- n
       let (Z :. w2  :. _) = extent w -- next layer size
       computeP 
         $ fromFunction (Z :. h1 :. w2)
         $ \ix -> b ! (Z :. col ix) + R.sumAllS 
                                       (R.zipWith (*)
                                       (slice i (Any :. row ix :. All))
                                       (slice w (Any :. col ix :. All)))
                  
-- TODO: Fuse it manually into mmultP to avoid allocation
-- TODO: use R.*^
-- A .* sigmoid'(B) (.* - elementWise multiplication)
{-# NOINLINE dotProdSigmP #-}       
dotProdSigmP :: Monad m => UMat -> UMat -> m UMat
dotProdSigmP a b = [a, b] `deepSeqArrays` computeP
                  $ fromFunction (extent a)
                  $ \ix -> a ! ix * sigmoid' (b ! ix)
                  
{-# INLINE outError #-}
outError :: Double -> Double -> Double -> Double
outError z a y = - (y - a) * sigmoid' z
                                 
-- returns lists of weighted sums and activations
forwardP :: Monad m => NN -> UMat -> m ([UMat], [UMat])
forwardP nn inp = liftM unzip $ forwardP' nn inp
  where
    forwardP' [] _ = return []
    forwardP' (l:ls) i = do
      z <- weightedSumsP i l
      a <- computeP $ R.map sigmoid z
      nxt <- forwardP' ls a
      return $ (z, a) : nxt
         
errorsP :: Monad m => UMat -> NN -> [UMat] -> [UMat] -> m [UMat]
errorsP y0 n0 z0 a0 = liftM reverse $ errors1 (reverse $ tail n0) (tail $ reverse z0) (last a0)
  where
    errors1 n' z' a' = do
      eOut <- computeP $ R.zipWith (-) a' y0
      eHid <- errorsHid eOut n' z'
      return (eOut : eHid)
    
    errorsHid nxt ((w,_):ls) (z:zs) = do 
      tmp <- mmultP nxt w
      cur <- dotProdSigmP tmp z
      prevs <- errorsHid cur ls zs
      return (cur : prevs)
    errorsHid _ _ _ = return []
      
-- ^ warning, (a:as) should contain input values as its head.
gradientP :: Monad m => [UMat] -> [UMat] -> m NN
gradientP (e:es) (a:as) = let m = (row $ extent e) in do
  enorm <- computeP $ R.map (/ fromIntegral m) e
  et <- transpose2P enorm
  fw <- mmultP et a
  fb <- sumP et
  nxt <- gradientP es as
  return ((fw, fb) : nxt)
gradientP _ _ = return []  

{-# INLINE sqDistPenalty #-}
sqDistPenalty :: Double -> Double -> Double
sqDistPenalty a y = let d = a - y in d * d

{-# INLINE logPenalty #-}
logPenalty :: Double -> Double -> Double
logPenalty a y = - y * log a - (1 - y) * log (1 - a)

hypothesis :: Monad m => NN -> UMat -> m UMat
hypothesis nn x = liftM (last . snd) $ forwardP nn x

-- ^ computes const and gradient using logPenalty cost function
costNGradient :: Monad m => NN -> UMat -> UMat -> m (Double, NN)
costNGradient nn x y = do
  let m = row $ extent y
  (z, a) <- forwardP nn x
  cost <- liftM (/ fromIntegral m) $ sumAllP $ R.zipWith logPenalty (last a) y
  e <- errorsP y nn z a
  grad <- gradientP e (x:a)
  return (cost, grad)