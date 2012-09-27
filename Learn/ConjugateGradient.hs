{-# LANGUAGE Rank2Types #-}

module Learn.ConjugateGradient where

import Debug.Trace
import Control.Monad

import Data.Maybe
import Data.Array.Repa as R hiding ((++))

import Learn.Types
import Learn.Optimization

dbg :: Show a => a -> a
dbg a = traceShow a a

dbgm :: Show a => String -> a -> a
dbgm m a = trace (m ++ ": " ++ show a) a

-- Underscores like in x_ means roughly "x value from previous iteration" whenever new x is calculated

-- TODO: maybe reduce parallelism somewhere? benchmark sumAllP vs. sumAllS, or decide in runtime depending on size

-- Constants:

-- ^ procedure to chose next step length. TODO: tune it
chose :: Double -> Double -> Double
chose low high = low + (high - low) / 100

-- ^ maximum step length, TODO: tune it
aMax :: Double
aMax = 1000.0

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
    x = fromMaybe 0.5 $ if a₀ < a₁
                        then interpolateMinNorm p₀ p₁ (m₀ * len) (m₁ * len)
                        else interpolateMinNorm p₀ p₁ (m₀ * len) (m₁ * len)

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
FIXME: fix cubic interpolation for interval with swapped bounds.
FIXME: add some checks if we approach machine precision limit. And do something about it.
-}

{-trace ("nans1: " ++ show (isNaN a₁ || isNaN a₂ || isNaN f₁ || isNaN f₁')) $ -}
{-trace ("nans1: " ++ show (hasNaNs x₂ || isNaN f₂ || isNaN f₂' || hasNaNs δf₂)) $ -}

hasNaNs :: UVec -> Bool
hasNaNs = isNaN . sumAllS

-- ^ line search algorithm form the book:
-- Jorge Nocedal, Stephen J. Wright, Numerical Optimization, Second Edition, Algorithm 3.5
lineSearch :: Monad m => Function m -> UVec -> UVec -> m (UVec, Double, UVec)
lineSearch fn start dir =
  do
    (_, f₀, f₀', δf₀) <- φ 0 -- TODO: value and derivative at starting point should already be calculated
    let step a₁ a₂ f₁ f₁' first = do
          (x₂, f₂, f₂', δf₂) <- trace ("nans1: " ++ show (isNaN a₁ || isNaN a₂ || isNaN f₁ || isNaN f₁')) $ φ a₂
          trace ("nans2: " ++ show (hasNaNs x₂) ++ show (isNaN f₂) ++ show (isNaN f₂') ++ show (hasNaNs δf₂)) $ 
            case () of 
              _ | f₂ > f₀ + c₁ * a₂ * f₀' || (f₂ > f₁ && not first) -> trace "zoom1" $ zoom a₁ a₂ f₁ f₂ f₁' f₂'
                | abs f₂' <= -c₂ * f₀' -> trace "gotcha in step" $ return (a₂, x₂, f₂, δf₂)
                | f₂' > 0 -> trace "zoom2" $ zoom a₂ a₁ f₂ f₁ f₂' f₁'
                | otherwise -> trace "recur" $ step a₂ (chose a₂ aMax) f₂ f₂' True

        zoom aLow aHigh fLow fHigh fLow' fHigh' = do
          --let aⱼ = cubicOrBisect aⱼ aHigh fLow fHigh fLow' fHigh'
          let aⱼ = dbg $ (aLow + aHigh) / 2
          (xⱼ, fⱼ, fⱼ', δfⱼ) <- φ aⱼ
          traceShow (aLow, fLow, fLow', aⱼ, fⱼ, fⱼ', aHigh, fHigh, fHigh') $
            case () of
              _ | fⱼ > f₀ + c₁ * aLow * f₀' || fⱼ >= fLow -> zoom aLow aⱼ fLow fⱼ fLow' fⱼ' -- move upper bound
                | abs fⱼ' <= - c₂ * f₀' -> return (aⱼ, xⱼ, fⱼ, δfⱼ)
                | fⱼ' * (aHigh - aLow) >= 0 -> zoom aLow aⱼ fLow fⱼ fLow' fⱼ' -- move upper bound
                | otherwise -> zoom aⱼ aHigh fⱼ fHigh fⱼ' fHigh' -- move lower bound

    (a, x, f, δf) <- trace ("calling step with f₀ = " ++ show f₀ ++ " and f₀' = " ++ show f₀') $ step 0 (chose 0 aMax) f₀  f₀' False
    let wolfe = if strongWolfe a f₀ f δf₀ δf dir then "PASS" else "FAIL"
    trace ("line search strong wolfe conditions checks: " ++ wolfe) $ return (x, f, δf)


  where
    φ a = do -- φ(a) = fn(start + a * dir), univariate representation of step length selection problem
      let x = computeS $ start +^ R.map (* a) dir
      (p, δp) <- fn x
      let p' = sumAllS $ δp *^ dir -- projection of gradient on search direction
      return (x, p, p', δp)

{-

dumb plug:

lineSearch :: Monad m => Function m -> UVec -> UVec -> m (UVec, Double, UVec)
lineSearch fn start dir = do
  (x, val, _, grad) <- φ 1
  return (x, val, grad)
  where
    φ a = do -- φ(a) = fn(start + a * dir), univariate representation of step length selection problem
      let x = computeS $ start +^ R.map (* a) dir
      (p, gp) <- fn x
      let p' = sumAllS $ gp *^ dir -- projection of gradient on search direction
      return (x, p, p', gp)
-}

-- Polack-Ribiere conjugate gradient method 
conjugateGradient :: Monad m => StopCondition -> Function m -> UVec -> m (UVec, Double)
conjugateGradient sc fn start = 
  do
    (_, f₀') <- fn start  -- get value and gradient at start
    let p₀ = computeS $ R.map negate f₀' -- initial search direction is the steepest descent direction
    loop start p₀ f₀' 0 0

  where
    (Z :. n) = extent start

    loop x_ p_ f_' i ir = do
      (x, f, f') <- lineSearch fn x_ p_
      let beta = betaPR f_' f'
          (betaPlus, restart) = trace ("beta is: " ++ show beta) $ if beta > 0 && ir < n -- if beta went below zero or at least every n'th iteration
                                                                   then (beta, False)    -- we restart using steepest descent direction
                                                                   else (0, True)
      let p = computeS $ R.map (* betaPlus) p_ -^ f'
      if checkIter sc i -- TODO: add tolerance test
        then return (x, f)
        else loop x p f' (i+1) (if restart then 0 else ir+1)

    betaPR prev cur = sumAllS (cur *^ cur) / sumAllS (prev *^ (prev{- -^ prev-}))
