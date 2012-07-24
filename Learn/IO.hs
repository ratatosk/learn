-- | Input-output in MatLab/Octave compatible text format.
--   Files written via these routines can be read in octave like this:
--   A = load("filename.txt");
--   And vice versa you can read files written like this:
--   dlmwrite("filename.txt", A)
--   Format is very simple: every row is written as one line of comma separated values without spaces

module Learn.IO where

import Prelude as P

import Control.Applicative ((<$>))

import Data.List

import Data.Array.Repa                          as R
import Data.Array.Repa.Repr.Unboxed             as R

writeRow :: (Show e, Source r e) => Array r DIM1 e -> String
writeRow r = intercalate "," $ P.map show $ toList r

writeMat :: (Show e, Source r e) => FilePath -> Array r DIM2 e -> IO ()
writeMat fn m = let (Z :. rn :. _) = extent m
                    rows = P.map (\n -> slice m (Any :. n :. All)) [0..rn - 1]
                in writeFile fn $ intercalate "\n" $ P.map writeRow rows

writeVec :: (Show e, Source r e) => FilePath -> Array r DIM1 e -> IO ()
writeVec fn v = writeMat fn $ extend (Any :. (1 :: Int) :. All) v 
    
commaSep :: String -> [String]
commaSep s = case break (== ',') s of 
  ([], _) -> []
  (x, xs) -> (x : commaSep (dropWhile (==',') xs))

-- TODO: use bytestrings and attoparsec to reduce memory usage (which is enormous)
  
readVals :: (Num e, Read e) => FilePath -> IO [e]
readVals fn = P.map read . concat . P.map commaSep . filter (not.null) . lines <$> readFile fn

-- ^ read matrix of known size
readMat :: (Num e, Read e, Unbox e) => FilePath -> Int -> Int -> IO (Array U DIM2 e)
readMat fn rows cols = fromListUnboxed (Z :. rows :. cols) <$> readVals fn

-- ^ read vector of known size
readVec :: (Num e, Read e, Unbox e) => FilePath -> Int -> IO (Array U DIM1 e)
readVec fn rows = fromListUnboxed (Z :. rows ) <$> readVals fn
