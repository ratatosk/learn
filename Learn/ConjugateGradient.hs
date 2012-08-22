module Learn.ConjugateGradient where

import Data.Array.Repa as R

import Learn.Types
import Learn.Optimization

-- ^ procedure to chose next step length. TODO: tune it
chose :: Double -> Double -> Double
chose min max = min + (max - min) / 10

-- ^ maximum step length, TODO: tune it
aMax :: Double
aMax = 1.0

-- ^ line search algorithm form the book:
-- Jorge Nocedal, Stephen J. Wright, Numerical Optimization, Second Edition, Algorithm 3.5
lineSearch :: Monad m => StopCondition -> Double -> Double -> Double -> Function -> UVec -> UVec -> m UVec
lineSearch sc c1 c2 fn start dir =
  do (p0, p0') <- phi 0 -- TODO: value and derivative at starting point should already be calculated
     step 0 (chose 0 aMax) p0  p0' False
  where
    phi a = do -- phi(a) = fn(start + a * dir), univariate representation of step length selection problem
      x <- computeP $ start +^ R.map(* a) dir
      (val, grad) <- fn x
      grad' <- foldAllP <$> grad *^ dir -- projection of gradient on search direction
      return (val, grad')
    step a1 a2 pa1 pa1' first = do
      (pa2, pa2') <- phi a2
      case () of _
                 | pa2 > p0 + c1 * a2 * p0' || (pa2 > pa1 && not first) -> zoom a1 a2
                 | abs pa2' <= -c2 * p0' -> return a2
                 | pa2' > 0 -> zoom a2 a1
                 | otherwise -> step a2 (chose a2 aMax) pa2 pa2' False
    zoom a1 a2 = ...
                   

  

conjugateGradient :: Monad m => StopCondition -> Function -> UVec -> m UVec
conjugateGradient = ...