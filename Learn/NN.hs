module Learn.NN where

import Data.Array.Repa as A

-- Number of neurons in every layer
type NNShape = [Int]

--            Input weights        Bias unit weights
type Layer = (Array U DIM2 Double, Array U DIM1 Double)

-- Neural network - list of layers.
type NN = [Layer]

randInit :: NNShape -> NN

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

sigmoid' :: Double -> Double
sigmoid' x = sx * (1 - sx) where sx = sigmoid x
                                 

