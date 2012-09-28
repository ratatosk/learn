-- | Reading files in MNIST format (http://yann.lecun.com/exdb/mnist/)
--   Writing is not implemented because it is hardly ever needed.
module Learn.IO.MNIST ( readVec
                      , readMat ) where

import Control.Monad

import System.IO

import qualified Data.ByteString as BS
import Data.Binary.Strict.Get

import Data.Array.Repa as R
import Data.Array.Repa.Repr.ByteString as R
import Data.Array.Repa.Repr.Unboxed as R

readVec :: (Unbox e, Num e) => FilePath -> Int -> IO (Array U DIM1 e)
readVec p s = withFile p ReadMode $ \h -> do
  header <- BS.hGet h 8
  let (Right (magic, nElems), _) = runGet (liftM2 (,) getWord32be getWord32be) header
  when (magic /= 2049) $ error "MNIST format error: bad magic number"
  when (s /= fromIntegral nElems) $ error "MNIST reading error: size mismatch"
  dat <- BS.hGetContents h
  let res = computeS $ R.map fromIntegral $ fromByteString (Z :. s) dat
  return $ deepSeqArray res res
    
readMat :: (Unbox e, Num e) => FilePath -> Int -> Int -> IO (Array U DIM2 e)
readMat p r c = withFile p ReadMode $ \h -> do
  header <- BS.hGet h 16
  let (Right (magic, nElems, ir, ic), _) =
        runGet (liftM4 (,,,) getWord32be getWord32be getWord32be getWord32be) header
  when (magic /= 2051) $ error "MNIST format error: bad magic number"
  when (r /= fromIntegral nElems || fromIntegral c /= ir * ic) $ error "MNIST reading error: size mismatch"
  dat <- BS.hGetContents h
  let res = computeS $ R.map fromIntegral $ fromByteString (Z :. r :. c) dat
  return $ deepSeqArray res res


    
            