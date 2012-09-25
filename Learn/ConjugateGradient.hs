module Learn.ConjugateGradient where

import Control.Monad

import Data.Array.Repa as R

import Learn.Types
import Learn.Optimization

-- Underscores like in x_ means roughly "x value from previous iteration" whenever new x is calculated

-- Constants:

-- ^ procedure to chose next step length. TODO: tune it
chose :: Double -> Double -> Double
chose min max = min + (max - min) / 10

-- ^ maximum step length, TODO: tune it
aMax :: Double
aMax = 1.0

-- ^ constants for strong Wolfe conditions recommended by Nocedal:
c1 :: Double
c1 = 0.0001
c2 :: Double
c2 = 0.1

-- Cubic Hermite spline based on values of function and its first derivatives
-- If there is no extremum in the interval or if it is not minimum, just bisect
-- Also if it is too close to the boundary, it is also rejected to prevent slow convergence.
interpolateMinNorm :: Double -> Double -> Double -> Double -> Maybe Double
interpolateMinNorm p0 p1 m0 m1 =
  let a = 2*p0 + m0 -2*p1 + m1
      b = -3*p0 - 2*m0 + 3*p1 - m1
      c = m0
      d = p1
      spline x = x*x*x*a + x*x*b + x*c + d
  in do
    cm <- cubicMin a b c
    guard $ x * (1 - x) >= 0.05 -- if it is less then zero, minimum is outside [0;1]
                               -- if it is just small, we are too close to the border
    guard $ spline x < min p0 p1 -- if value at the boundary is less than value at extremum

-- normalize values as if we have values of function and tangent at 0 and 1, then find minimum
cubicOrBisect :: Double -> Double -> Double -> Double -> Double -> Double -> Double
cubicOrBisect a0 a1 p0 p1 m0 m1 = a0 + x * len
  where
    len = a1 - a0
    x = fromMaybe 0.5 $ interpolateMinNorm p0 p1 (m0 * len) (m1 * len)

-- find minimum of cubic polynomial (last coefficient is omitted)
-- to avoid endless loops somewhere in the calling code (zoom)
cubicMin :: Double -> Double -> Double -> Maybe Double
cubicMin a b c = if det > 0
                 then Just $ (-b + sqrt det) / (3 * a) -- determinant is taken with plus for minimum
                 else Nothing
  where
    det = b*b-3*a*c

-- ^ line search algorithm form the book:
-- Jorge Nocedal, Stephen J. Wright, Numerical Optimization, Second Edition, Algorithm 3.5
lineSearch :: Monad m => Function -> UVec -> UVec -> m UVec
lineSearch fn start dir =
  do (p0, p0') <- phi 0 -- TODO: value and derivative at starting point should already be calculated
     step 0 (chose 0 aMax) p0  p0' False
  where
    phi a = do -- phi(a) = fn(start + a * dir), univariate representation of step length selection problem
      x <- computeP $ start +^ R.map (* a) dir
      (val, grad) <- fn x
      grad' <- sumAllP $ grad *^ dir -- projection of gradient on search direction
      return (val, grad')

    step a1 a2 pa1 pa1' first = do
      (pa2, pa2') <- phi a2
      case () of _
                 | pa2 > p0 + c1 * a2 * p0' || (pa2 > pa1 && not first) -> zoom a1 a2
                 | abs pa2' <= -c2 * p0' -> return a2
                 | pa2' > 0 -> zoom a2 a1
                 | otherwise -> step a2 (chose a2 aMax) pa2 pa2' False

    zoom al ah pl ph pl' ph' = do
      let aj = cubicOrBisect al ah pl ph pl' ph'
      (pj, pj') <- phi aj
      case () of _
                   | pj > p0 + cl * al * p0' || pj >= pl -> zoom al aj pl pj pl' pj' -- move upper bound
                   | abs pj' <= - c2 * p0' -> return aj
                   | pj' * (ah - al) >= 0 -> zoom al aj pl pj pl' pj' -- move upper bound
                   | otherwise -> zoom aj ah pj ph pj' ph' -- move lower bound

-- Polack-Ribiere conjugate method 
conjugateGradient :: Monad m => StopCondition -> Function -> UVec -> m UVec
conjugateGradient sc fn start = let (Z :. n) = extent start in
  do
    (f0, f0') <- fn start  -- get value and gradient at start
    f0'norm <- sumAllP $ f0' *^ f0'
    let p0 = R.map neg f0' -- initial search direction is the steepest descent direction
    loop start p0 f0' 0
  where 
    loop x_ p_ f_' i = do
      a <- lineSearch fn x_ p
      x <- computeP $ x +^ R.map (* a) p -- TODO: it is actually computed during line search
      (f, f') <- fn x
      f_'norm <- sumAllP $ f_' *^ f_'
      betNom <- sumAllP $ f' *. (f' -^ f_')
      let beta = betNom / f_'norm
          (betaPlus, restarted) = if beta > 0 || ir >= n -- if beta went below zero or at least every n'th iteration
                                  then (beta, False)    -- we restart using use steepest descent direction
                                  else (0, True) 
      p <- computeP $ R.map (* beta) p_ -^ f'
      if checkIter sc i
        then return x
        else loop x p f' (i+1) (if restarted then 0 else ir+1)
          
    