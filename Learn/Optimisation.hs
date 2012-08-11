module Learn.Optimization where

import Learn.Types

-- ^ Funtcion to feed to optimisation algorithm: returns function value and partial derivatives
type Function = UVec -> (Double, UVec)
                
-- ^ Optimization stop conditions
data StopCondition = StopCondition { tol :: Maybe (Double -> Double -> Bool) -- ^ decide based on two consecutive function values
                                   , maxIter :: Maybe Int -- ^ maximum number of optimization steps
                                   }
