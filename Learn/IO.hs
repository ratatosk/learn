-- | Input-output in MatLab/Octave compatible text format.
--   Files written via these routines can be read in octave like this:
--   A = load("filename.txt");
--   And vice versa you can read files written like this:
--   dlmwrite("filename.txt", A)
--   Format is very simple: every row is written as one line of comma separated values without spaces

module Learn.IO where

import Prelude as P

import Control.Applicative ((<$>))
import Control.Monad ((>=>))

import Data.List

import System.IO

import Data.Array.Repa                          as R
import Data.Array.Repa.Repr.Unboxed             as R

writeRow :: (Show e, Source r e) => Handle -> Array r DIM1 e -> IO ()
writeRow h r = hPutStrLn h $ intercalate ", " $ P.map show $ toList r

writeMat :: (Show e, Source r e) => FilePath -> Array r DIM2 e -> IO ()
writeMat fn m = let (Z :. rn :. _) = extent m
                    rows = P.map (\n -> slice m (Any :. n :. All)) [0..rn - 1]
                in do 
                  h <- openFile fn WriteMode
                  mapM_ (writeRow h) rows
  
writeVec :: (Show e, Source r e) => FilePath -> Array r DIM1 e -> IO ()
writeVec fn v = writeMat fn $ extend (Any :. (1::Int) :. All) v 
    
commaSep :: String -> [String]
commaSep s = case break (== ',') s of 
  ([], _) -> []
  (x, xs) -> (x : commaSep (dropWhile (==',') xs))

readVals :: (Num e, Read e) => FilePath -> IO [e]
readVals fn = withFile fn ReadMode $ hGetContents >=> return . P.map read . concat . P.map commaSep . filter (not.null) . lines

-- ^ read matrix of known size
readMat :: (Num e, Read e, Unbox e) => FilePath -> Int -> Int -> IO (Array U DIM2 e)
readMat fn rows cols = fromListUnboxed (Z :. rows :. cols) <$> readVals fn

-- ^ read vector of known size
readVec :: (Num e, Read e, Unbox e) => FilePath -> Int -> IO (Array U DIM1 e)
readVec fn rows = fromListUnboxed (Z :. rows ) <$> readVals fn