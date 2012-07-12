{-# LANGUAGE TupleSections #-}

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
-- i - input
-- w - weight matrix
-- b - bias weights vector
-- y - labels (ground truth)

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
    do let (Z :. h1  :. _) = extent i
       let (Z :. w2  :. _) = extent w
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
forwardP l i = liftM unzip $ forwardP' l i
  where
    forwardP' [] _ = return []
    forwardP' (l:ls) i = do
      z <- weightedSumsP i l
      a <- computeP $ R.map sigmoid z
      nxt <- forwardP' ls a
      return $ (z, a) : nxt
         
errorsP :: Monad m => UMat -> NN -> [UMat] -> [UMat] -> m [UMat]
errorsP y n z a = errors1 y (reverse $ tail n) (tail $ reverse z) (last a)
  where
    errors1 y' n' z' a' = do
      eOut <- computeP $ R.zipWith (-) y' a'
      eHid <- errorsHid eOut n' z'
      return (eOut : eHid)
    
    errorsHid _ [] _ = return []
    errorsHid nxt ((w,_):ls) (z:zs) = do 
      tmp <- mmultP nxt w
      cur <- dotProdSigmP tmp z
      prevs <- errorsHid cur ls zs
      return (cur : prevs)
      
gradientP :: Monad m => [UMat] -> [UMat] -> m NN
gradientP [] _ = return []
gradientP (e:es) (a:as) = let m = (row $ extent e) in do
  enorm <- computeP $ R.map (/ fromIntegral m) e
  et <- transpose2P enorm
  fw <- mmultP et a
  fb <- sumP et
  nxt <- gradientP es as
  return ((fw, fb) : nxt)
  
costP :: Monad m => NN -> UMat -> UMat -> m Double
costP n i y = do
  let m = row $ extent y
  (_, a) <- forwardP n i
  s <- sumAllP $ R.map (\x -> x*x) $ R.zipWith (-) y (last a)
  return $ s / fromIntegral m

