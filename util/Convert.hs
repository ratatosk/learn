module Main where

import Data.Maybe

import Data.Array.Repa
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.IO.Binary

import System.Environment
import System.Exit
import System.FilePath.Posix

import Learn.IO

txt2bin :: FilePath -> Int -> Int -> IO ()
txt2bin fn r c = do
  mat <- readMat fn r c :: IO (Array U DIM2 Double)
  writeArrayToStorableFile fn mat
  
bin2txt :: FilePath -> Int -> Int -> IO ()
bin2txt fn r c = do
  mat <- readArrayFromStorableFile fn (Z :. r :. c) :: IO (Array F DIM2 Double)
  writeMat fn mat
  

convert :: FilePath -> Int -> Int -> IO ()
convert fn r c = case takeExtension fn of
  ".txt" -> txt2bin fn r c
  ".bin" -> bin2txt fn r c
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
