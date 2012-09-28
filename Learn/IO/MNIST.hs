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

readArr :: (Unbox e, Num e, Shape sh) => FilePath -> sh -> IO (Array U sh e)
readArr path shape = withFile path ReadMode $ \h -> do
  header <- BS.hGet h 8
  let (Right (enc, nElems), _) = runGet (liftM2 (,) getWord32be getWord32be) header
  when (enc /= 2049) $ error "MNIST format error: bad magic number"
  when (size shape /= fromIntegral nElems) $ error "MNIST reading error: size mismatch"
  dat <- BS.hGetContents h
  let res = computeS $ R.map fromIntegral $ fromByteString shape dat
  return $ deepSeqArray res res

readVec :: (Unbox e, Num e) => FilePath -> Int -> IO (Array U DIM1 e)
readVec p s = readArr p (Z :. s)

readMat :: (Unbox e, Num e) => FilePath -> Int -> Int -> IO (Array U DIM2 e)
readMat p r c = readArr p (Z :. r :. c)


    
            