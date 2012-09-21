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

interpolateNormMin x1

-- Cubic Hermite spline based on values of function and its first derivatives
-- If there is no extremum in the interval or if it is not minimum, just bisect
interpolateMinNorm :: Double -> Double -> Double -> Double -> Maybe Double
interpolateMinNorm p0 p1 m0 m1 = do
  cm <- cubicMin a b c
  let (minBound, minBoundVal) = if p0 < p1 then (0, p0) else (1, p1)
      a = 2*p0 + m0 -2*p1 + m1
      b = -3*p0 - 2*m0 + 3*p1 - m1
      c = m0
      d = p1
      spline x = x*x*x*a + x*x*b + x*c + d
  if spline x < minBoundVal then return x else Nothing

-- normalize values as if we have values of function and tangent at 0 and 1, then find minimum
cubicOrBisect :: Double -> Double -> Double -> Double -> Double -> Double -> Double
cubicOrBisect a0 a1 p0 p1 m0 m1 = a0 + x * len
  where
    len = a1 - a0
    x = fromMaybe 0.5 $ interpolateMinNorm p0 p1 (m0 * len) (m1 * len)

-- find minimum of cubic polynomial (last coefficient is omitted)
cubicMin :: Double -> Double -> Double -> Maybe Double
cubicMin a b c = if det > 0
                 then Just $ (-b + sqrt det) / (3 * a) -- determinant is taken with plus for minimum
                 else Nothing
  where
    det = b*b-3*a*c

-- ^ line search algorithm form the book:
-- Jorge Nocedal, Stephen J. Wright, Numerical Optimization, Second Edition, Algorithm 3.5
lineSearch :: Monad m => StopCondition -> Double -> Double -> Double -> Function -> UVec -> UVec -> m UVec
lineSearch sc c1 c2 fn start dir =
  do (p0, p0') <- phi 0 -- TODO: value and derivative at starting point should already be calculated
     step 0 (chose 0 aMax) p0  p0' False
  where
    phi a = do -- phi(a) = fn(start + a * dir), univariate representation of step length selection problem
      x <- computeP $ start +^ R.map(* a) di
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

    zoom a1 a2 pa1 pa2 pa1' pa2' =

conjugateGradient :: Monad m => StopCondition -> Function -> UVec -> m UVec
conjugateGradient = ...