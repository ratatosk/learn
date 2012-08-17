module Learn.ConjugateGradient where

import Data.Array.Repa as R

import Learn.Types
import Learn.Optimization
import Learn.Algorithms

-- ^ procedure to chose next step length
chose :: Double -> Double -> Double
chose min max = min + (max - min) / 10

-- ^ maximum step length, TODO: tune it
aMax :: Double
aMax = 1.0

-- ^ line search algorithm form the book:
-- Jorge Nocedal, Stephen J. Wright, Numerical Optimization, Second Edition, Algorithm 3.5
lineSearch :: Monad m => StopCondition -> Double -> Double -> Double -> Function -> UVec -> UVec -> m UVec
lineSearch sc c1 c2 fn start dir =
  do (f0, f'0) <- fn start -- TODO: value and derivative at starting point should already be calculated
     step 0 (chose 0 aMax)
  where
    step a1 a2 = do
      x2 <- computeP $ start +^ R.map (* a2) dir
      (fa, f'a) <- fn x2
      
      case () of _
                 | fa > f0 + c1 * a1

  

conjugateGradient :: Monad m => StopCondition -> Function -> UVec -> m UVec
conjugateGradient = ...