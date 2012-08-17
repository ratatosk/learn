module Learn.Algorithms where

import Control.Applicative ((<$>))
import Data.Array.Repa

normP :: (Monad m, Shape sh, Source r a, Elt a, Unbox a) => Array r sh a -> a
normP x = dotProductP x x

dotProductP :: (Monad m, Shape sh, Source r a, Elt a, Unbox a) => Array r sh a -> Array r sh a -> a
dotProductP x y = foldAllP <$> x *^ y