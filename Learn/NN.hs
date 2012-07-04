module Learn.NN where

import Data.Array.Repa as A

-- Number of neurons in every layer
type NNShape = [Int]

type UMat = Array U DIM2 Double
type UVec = Array U DIM1 Double

-- Input weights and bias unit weights
type Layer = (UMat, UVec)

-- Neural network - list of layers.
type NN = [Layer]

randInit :: NNShape -> NN

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

sigmoid' :: Double -> Double
sigmoid' x = sx * (1 - sx) where sx = sigmoid x
                                 
-- returns weighted sums and activations
activations :: UMat -> NN -> [(UMat, UMat)]

