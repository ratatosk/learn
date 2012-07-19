module Main where

import Control.Applicative

import Learn.IO

import System.Environment

main :: IO ()
main = do
  fn <- head <$> getArgs
  v <- readVals fn :: IO [Double]
  print $ length v