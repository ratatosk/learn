{-# LANGUAGE Rank2Types #-}

module Learn.Optimization where

import Learn.Types

-- ^ Funtcion to feed to optimisation algorithm: returns function value and partial derivatives
type Function = Monad m => UVec -> m (Double, UVec)
                
-- ^ Optimization stop conditions
data StopCondition = StopCondition { tol :: Maybe (Double -> Double -> Bool) -- ^ decide based on two consecutive function values
                                   , maxIter :: Maybe Int -- ^ maximum number of optimization steps
                                   }

checkIter :: StopCondition -> Int -> Bool
checkIter StopCondition {maxIter = Just m} i = i >= m
checkIter _ _ = False

checkTol :: StopCondition -> Double -> Double -> Bool
checkTol StopCondition {tol = Just t} prev cur = t prev cur
checkTol _ _ _ = False