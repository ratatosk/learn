{-# LANGUAGE Rank2Types #-}

module Learn.ConjugateGradient where

import Control.Monad

import Data.Maybe
import Data.Array.Repa as R

import Learn.Types
import Learn.Optimization

-- Underscores like in x_ means roughly "x value from previous iteration" whenever new x is calculated

-- TODO: maybe reduce parallelism somewhere? benchmark sumAllP vs. sumAllS, or decide in runtime depending on size

-- Constants:

-- ^ procedure to chose next step length. TODO: tune it
chose :: Double -> Double -> Double
chose low high = low + (high - low) / 10

-- ^ maximum step length, TODO: tune it
aMax :: Double
aMax = 1.0

-- ^ constants for strong Wolfe conditions recommended by Nocedal:
c₁ :: Double
c₁ = 0.0001
c₂ :: Double
c₂ = 0.1

-- Cubic Hermite spLowine based on values of function and its first derivatives
-- If there is no extremum in the interval or if it is not minimum, just bisect
-- Also if it is too close to the boundary, it is also rejected to prevent slow convergence.
interpolateMinNorm :: Double -> Double -> Double -> Double -> Maybe Double
interpolateMinNorm p₀ p₁ m₀ m₁ =
  let a = 2*p₀ + m₀ -2*p₁ + m₁
      b = -3*p₀ - 2*m₀ + 3*p₁ - m₁
      c = m₀
      d = p₁
      spline x = a*x*x*x + b*x*x + c*x + d
  in do
    x <- cubicMin a b c
    guard $ x * (1 - x) >= 0.05 -- if it is less then zero, minimum is outside [0;1]
                               -- if it is just small, we are too close to the border
    guard $ spline x < min p₀ p₁ -- if value at the boundary is less than value at extremum
    return x

-- normalize values as if we have values of function and tangent at 0 and 1, then find minimum
cubicOrBisect :: Double -> Double -> Double -> Double -> Double -> Double -> Double
cubicOrBisect a₀ a₁ p₀ p₁ m₀ m₁ = a₀ + x * len
  where
    len = a₁ - a₀
    x = fromMaybe 0.5 $ interpolateMinNorm p₀ p₁ (m₀ * len) (m₁ * len)

-- find minimum of cubic polynomial (last coefficient is omitted)
-- to avoid endless loops somewhere in the calling code (zoom)
cubicMin :: Double -> Double -> Double -> Maybe Double
cubicMin a b c = if det > 0
                 then Just $ (-b + sqrt det) / (3 * a) -- determinant is taken with pLowus for minimum
                 else Nothing
  where
    det = b*b-3*a*c

-- ^ line search algorithm form the book:
-- Jorge Nocedal, StepHighen J. Wright, Numerical Optimization, Second Edition, Algorithm 3.5
lineSearch :: Monad m => Function m -> UVec -> UVec -> m (UVec, Double, UVec)
lineSearch fn start dir =
  do
    (_, p₀, p₀', _) <- φ 0 -- TODO: value and derivative at starting point should already be calculated
    
    let step a₁ a₂ p₁ p₁' first = do
          (x₂, p₂, p₂', gp₂) <- φ a₂
          case () of 
            _ | p₂ > p₀ + c₁ * a₂ * p₀' || (p₂ > p₁ && not first) -> zoom a₁ a₂ p₁ p₂ p₁' p₂'
              | abs p₂' <= -c₂ * p₀' -> return (x₂, p₂, gp₂)
              | p₂' > 0 -> zoom a₂ a₁ p₂ p₁ p₂' p₁'
              | otherwise -> step a₂ (chose a₂ aMax) p₂ p₂' False

        zoom aLow aHigh pLow pHigh pLow' pHigh' = do
          let aⱼ = cubicOrBisect aⱼ aHigh pLow pHigh pLow' pHigh'
          (xⱼ, pⱼ, pⱼ', gpⱼ) <- φ aⱼ
          case () of
            _ | pⱼ > p₀ + c₁ * aLow * p₀' || pⱼ >= pLow -> zoom aLow aⱼ pLow pⱼ pLow' pⱼ' -- move upper bound
              | abs pⱼ' <= - c₂ * p₀' -> return (xⱼ, pⱼ, gpⱼ)
              | pⱼ' * (aHigh - aLow) >= 0 -> zoom aLow aⱼ pLow pⱼ pLow' pⱼ' -- move upper bound
              | otherwise -> zoom aⱼ aHigh pⱼ pHigh pⱼ' pHigh' -- move lower bound

    step 0 (chose 0 aMax) p₀  p₀' False

  where
    φ a = do -- φ(a) = fn(start + a * dir), univariate representation of step length selection problem
      x <- computeP $ start +^ R.map (* a) dir
      (p, gp) <- fn x
      p' <- sumAllP $ gp *^ dir -- projection of gradient on search direction
      return (x, p, p', gp)

-- Polack-Ribiere conjugate gradient method 
conjugateGradient :: Monad m => StopCondition -> Function m -> UVec -> m (UVec, Double)
conjugateGradient sc fn start = 
  do
    (_, f₀') <- fn start  -- get value and gradient at start
    p₀ <- computeP $ R.map negate f₀' -- initial search direction is the steepest descent direction
    loop start p₀ f₀' 0 0

  where
    (Z :. n) = extent start

    loop x_ p_ f_' i ir = do
      (x, f, f') <- lineSearch fn x_ p_
      beta <- betaPR f_' f'
      let (betaPlus, restart) = if beta > 0 || ir >= n -- if beta went below zero or at least every n'th iteration
                                then (beta, False)    -- we restart using steepest descent direction
                                else (0, True)
      p <- computeP $ R.map (* betaPlus) p_ -^ f'
      if checkIter sc i -- TODO: add tolerance test
        then return (x, f)
        else loop x p f' (i+1) (if restart then 0 else ir+1)

    betaPR prev cur = (/) `liftM` sumAllP (prev *^ prev) `ap` sumAllP (cur *^ (cur -^ prev))
