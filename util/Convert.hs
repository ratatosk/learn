module Main where

import Data.Maybe

import Data.Array.Repa hiding ((++))
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.IO.Binary

import System.Environment
import System.Exit
import System.FilePath.Posix

import Learn.IO

txt2bin :: FilePath -> FilePath -> Int -> Int -> IO ()
txt2bin from to r c = do
  mat <- readMat from r c :: IO (Array U DIM2 Double)
  writeArrayToStorableFile to mat
  
bin2txt :: FilePath -> FilePath -> Int -> Int -> IO ()
bin2txt from to r c = do
  mat <- readArrayFromStorableFile from (Z :. r :. c) :: IO (Array F DIM2 Double)
  writeMat to mat
  

convert :: FilePath -> Int -> Int -> IO ()
convert fn r c = case splitExtension fn of
  (n, ".txt") -> txt2bin fn (n ++ ".bin") r c
  (n, ".bin") -> bin2txt fn (n ++ ".txt") r c
  _ -> do 
    putStrLn "unknown file type"
    exitFailure
    
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

main :: IO ()
main = do
  a <- getArgs
  case a of
    [r,c,fn] -> case (maybeRead r, maybeRead c) of
      (Just rows, Just cols) -> convert fn rows cols
      _ -> do
           putStrLn "failed to parse number of rows or columns"
           exitFailure
    _ -> do
      putStrLn "usage: convert <rows> <columns> <filename>"
      putStrLn "extension may be .txt or .bin"
      exitFailure
