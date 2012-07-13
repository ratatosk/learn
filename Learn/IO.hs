-- | Input-output in MatLab/Octave compatible text format.
--   Files written via these routines can be read in octave like this:
--   A = load("filename.txt");
--   And vice versa you can read files written like this:
--   dlmwrite("filename.txt", A)
--   Format is very simple: every row is written as one line of comma separated values without spaces

module Learn.IO where

import Prelude as P

import Control.Applicative

import Data.List

import System.IO

import Data.Array.Repa                          as R
import Data.Array.Repa.Repr.Unboxed             as R
{-
prependToAllM_ :: (Monad m) => m () -> [m ()] -> m ()
prependToAllM_ _ [] = return ()
prependToAllM_ d (a:as) = d >> a >> prependToAllM_ d as

intersperseM_ :: (Monad m) => m () -> [m ()] -> m ()
intersperseM_ _ [] = return ()
intersperseM_ d (a:as) = a >> prependToAllM_ d as
-} 

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
  
--readRow :: (Num e, Read e, Unbox 

readMat :: (Num e, Read e, Unbox e) => FilePath -> IO (Array U DIM2 e)
readMat fn = do
  h <- openFile fn ReadMode
  ls <- lines <$> hGetContents h
  let rows = length ls
      sls = P.map commaSep ls
      cols = length $ head ls
      vals = P.map read $ concat sls
  return $ fromListUnboxed (Z :. cols :. rows) vals
  
{-
readVec :: (Num e, Read e, Unbox e) => FilePath -> IO Array U DIM1 Double
-}
