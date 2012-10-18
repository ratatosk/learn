{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module Main where

import Control.Applicative
import Control.Monad

import System.Environment
import System.Exit
import System.FilePath.Posix

import Data.Array.Repa as R hiding ((++))

import Learn.IO.Text
import Learn.IO.MNIST

data MemType = MemWord8 | MemDouble
data MemDim = One | Two | Three

class Dimension a where
  dim ::

{-
class MemState t d where
  data Repr :: * -> * -> *
-}

instance 

main :: IO ()
main = print "Hello world"
