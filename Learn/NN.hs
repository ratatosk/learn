{-# LANGUAGE ExistentialQuantification, Rank2Types, FlexibleContexts #-}

module Learn.NN where

import Prelude as P

import Control.Monad (liftM)

import Data.Array.Repa                  as R
--import Data.Array.Repa.Unsafe           as R

import Data.Array.Repa.Algorithms.Matrix
import Data.Array.Repa.Algorithms.Randomish

import Learn.Types
import Learn.Optimization

-- Number of neurons in every layer
type NNShape = [Int]

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

deepSeqLayer :: Layer -> b -> b
deepSeqLayer (w, b) x = w `deepSeqArray` b `deepSeqArray` x

deepSeqNN :: NN -> b -> b
deepSeqNN [] x = x
deepSeqNN (l:ls) x = deepSeqLayer l $ deepSeqNN ls x

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

-- ^ computes logPenalty based cost function
cost :: Monad m => NN -> UMat -> UMat -> m Double
cost nn x y = do
  let m = row $ extent y
  (_, a) <- forwardP nn x
  liftM (/ fromIntegral m) $ sumAllP $ R.zipWith logPenalty (last a) y

-- ^ computes const and gradient using logPenalty cost function
costNGradient :: Monad m => NN -> UMat -> UMat -> m (Double, NN)
costNGradient nn x y = do
  let m = row $ extent y
  (z, a) <- forwardP nn x
  c <- liftM (/ fromIntegral m) $ sumAllP $ R.zipWith logPenalty (last a) y
  e <- errorsP y nn z a
  grad <- gradientP e (x:a)
  return (c, grad)

-- omg...
mapPair :: (Shape sh1, Shape sh2) => (forall sh' . Shape sh' => Array U sh' Double -> Array U sh' Double -> Array U sh' Double) -> (Array U sh1 Double, Array U sh2 Double) -> (Array U sh1 Double, Array U sh2 Double) -> (Array U sh1 Double, Array U sh2 Double)
mapPair f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

-- ^ sum network weight and bias units with gradients
nnSumS :: NN -> NN -> Double -> NN
nnSumS nn grad alpha = P.zipWith (mapPair $ (\m1 m2 -> computeS $ R.zipWith (\a b -> a + alpha * b) m1 m2)) nn grad

mat2Vec :: Source r e => Array r DIM2 e -> Array D DIM1 e
mat2Vec m = let (Z :. r :. c) = extent m in reshape (Z :. r * c) m
    
vec2Mat :: Source r e => Int -> Int -> Array r DIM1 e -> Array D DIM2 e
vec2Mat r c m = reshape (Z :. r :. c) m

layer2Vec :: Layer -> Array D DIM1 Double
layer2Vec (w, b) = R.append (mat2Vec w) b

-- ^ convert vector to NN layer given fan-in, fan-out and offset from the beginning of the vector
vec2Layer :: Source r Double => Int -> Int -> Int ->  Array r DIM1 Double -> Layer
vec2Layer i o s v =
  let w = vec2Mat o i $ extract (Z :. s) (Z :. o * i) v
      b = extract (Z :. o * i + s) (Z :. o) v
  in (computeS w, computeS b)

-- ^ unroll all neural network parameters to single vector - needed for advanced optimization algorithms
nn2Vector :: NN -> UVec
nn2Vector nn = computeS $ foldr1 R.append $ P.map layer2Vec nn

vector2NN :: NNShape -> UVec -> NN
vector2NN sh v =
  let starts = 0 : (scanl1 (+) $ P.map (\(i, o) -> i * o + o) $ zip sh $ tail sh)
  in P.map (\(s, i, o) -> vec2Layer i o s v) $ zip3 starts (init sh) (tail sh)

-- ^ convert training set to optimization target
ts2Function :: NNShape -> UMat -> UMat -> Function
ts2Function sh x y nnVec = do
  (c, nnGrad) <- costNGradient (vector2NN sh nnVec) x y
  return (c, nn2Vector nnGrad)
