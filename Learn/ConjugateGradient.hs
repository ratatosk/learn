{-# LANGUAGE Rank2Types #-}

module Learn.ConjugateGradient where

import Control.Monad

import Data.Maybe
import Data.Array.Repa as R hiding ((++))

import Learn.MonadLogger
import Learn.Types
import Learn.Optimization

-- Underscores like in x_ means roughly "x value from previous iteration" whenever new x is calculated

-- TODO: maybe reduce parallelism somewhere? benchmark sumAllP vs. sumAllS, or decide in runtime depending on size

-- Constants:

-- ^ procedure to chose next step length. TODO: tune it
chose :: Double -> Double -> Double
chose low high = low + (high - low) / 100

-- ^ maximum step length, TODO: tune it
aMax :: Double
aMax = 50.0

-- ^ constants for strong Wolfe conditions recommended by Nocedal:
c₁ :: Double
c₁ = 0.0001
c₂ :: Double
c₂ = 0.1

-- Cubic Hermite spline based on values of function and its first derivatives
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

-- ^ Strong Wolfe conditions check, for testing line search algorithms
strongWolfe :: Double -> Double -> Double -> UVec -> UVec -> UVec -> Bool
strongWolfe a f₀ f δf₀ δf p =
  f <= f₀ + c₁ * a * sumAllS (δf₀ *^ p) &&                 -- sufficient decrease
  abs (sumAllS (δf *^ p)) <= abs (c₂ * sumAllS (δf₀ *^ p)) -- curvature


{-
FIXME: add some checks if we approach machine precision limit. And do something about it.
-}

hasNaNs :: UVec -> Bool
hasNaNs = isNaN . sumAllS

-- ^ line search algorithm form the book:
-- Jorge Nocedal, Stephen J. Wright, Numerical Optimization, Second Edition, Algorithm 3.5
lineSearch :: MonadLogger m => Function m -> UVec -> Double -> UVec -> UVec -> m (UVec, Double, UVec)
lineSearch fn x₀ f₀ δf₀ dir =
  do
    let f₀' = sumAllS $ δf₀ *^ dir
        step a₁ a₂ f₁ f₁' first = do
          yell $ "line search step with a1 = " ++ show a₁ ++ "; a2 = " ++ show a₂
          (x₂, f₂, f₂', δf₂) <-  φ a₂
          case () of
            _ | f₂ > f₀ + c₁ * a₂ * f₀' || (f₂ > f₁ && not first) -> zoom a₁ a₂ f₁ f₂ f₁' f₂'
              | abs f₂' <= -c₂ * f₀' -> return (x₂, f₂, δf₂)
              | f₂' > 0 -> zoom a₂ a₁ f₂ f₁ f₂' f₁'
              | otherwise -> step a₂ (chose a₂ aMax) f₂ f₂' True

        zoom aLow aHigh fLow fHigh fLow' fHigh' = do
          yell $ "line search zoom with al = " ++ show aLow ++ "; ah = " ++ show aHigh
          let aⱼ = if aLow < aHigh
                   then cubicOrBisect aLow aHigh fLow fHigh fLow' fHigh'
                   else cubicOrBisect aHigh aLow fHigh fLow fHigh' fLow'
          (xⱼ, fⱼ, fⱼ', δfⱼ) <- φ aⱼ
          case () of
            _ | fⱼ > f₀ + c₁ * aLow * f₀' || fⱼ >= fLow -> zoom aLow aⱼ fLow fⱼ fLow' fⱼ' -- move upper bound
              | abs fⱼ' <= - c₂ * f₀' -> return (xⱼ, fⱼ, δfⱼ)
              | fⱼ' * (aHigh - aLow) >= 0 -> zoom aLow aⱼ fLow fⱼ fLow' fⱼ' -- move upper bound
              | otherwise -> zoom aⱼ aHigh fⱼ fHigh fⱼ' fHigh' -- move lower bound

    yell $ "initial gradient: " ++ show f₀'
    step 0 (chose 0 aMax) f₀  f₀' False
    


  where
    φ a = do -- φ(a) = fn(x₀ + a * dir), univariate representation of step length selection problem
      let x = computeS $ x₀ +^ R.map (* a) dir
      (p, δp) <- fn x
      let p' = sumAllS $ δp *^ dir -- projection of gradient on search direction
      return (x, p, p', δp)

-- Polack-Ribiere conjugate gradient method with restarts
conjugateGradient :: MonadLogger m => StopCondition -> Function m -> UVec -> m (UVec, Double)
conjugateGradient sc fn x₀ =
  do
    (f₀, δf₀) <- fn x₀  -- get value and gradient at start
    let p₀ = computeS $ R.map negate δf₀ -- initial search direction is the steepest descent direction
    loop x₀ f₀ δf₀ p₀ 0 0

  where
    (Z :. n) = extent x₀

    loop x_ f_ δf_ p_ i ir = do
      yell $ "conjugate gradient starting iteration " ++ show i
      (x, f, δf) <- lineSearch fn x_ f_ δf_ p_
      let beta = betaPR δf_ δf
          (betaPlus, restart) = if beta > 0 && ir < n -- if beta went below zero or at least every n'th iteration
                                then (beta, False)    -- we restart using steepest descent direction
                                else (0, True)
      let p = computeS $ R.map (* betaPlus) p_ -^ δf
      if checkIter sc i || checkTol sc f_ f
        then return (x, f)
        else loop x f δf p (i+1) (if restart then 0 else ir+1)

    betaPR prev cur = sumAllS (cur *^ cur) / sumAllS (prev *^ (prev -^ cur))
