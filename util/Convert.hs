module Main where

import Data.Array.Repa
import Data.Array.IO.Binary

import System.Environment
import System.Exit
import System.FilePath.Posix

import Learn.IO

txt2bin :: FilePath -> IO ()
txt2bin fn = do
  
  
  
bin2txt :: FilePath -> IO ()
bin2txt fn = do

convert :: FileName -> IO ()
convert fn = case takeExtension fn of
  ".txt" -> txt2bin fn
  ".bin" -> bin2txt fn
  _ -> do 
    putStrLn "unknown file type"
    exitFailure
    

main :: IO ()
main = do
  a <- getArgs
  case a of
    [dim,fn] -> convert dim fn
    _ -> do
      putStrLn "usage: convert <filename>"
      putStrLn "extension may be .txt or .bin"
      exitFailure