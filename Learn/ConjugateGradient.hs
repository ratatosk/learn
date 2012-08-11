module Learn.ConjugateGradient where

import Data.Array.Repa

import Learn.Types
import Learn.Optimization

conjugateGradient :: Function -> StopCondition -> UVec -> UVec
conjugateGradient 