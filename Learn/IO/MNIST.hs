-- | Reading files in MNIST format (http://yann.lecun.com/exdb/mnist/)
--   Writing is not implemented because it is hardly ever needed.
module Learn.IO.MNIST ( readLabels
                      , readImages
                      , readLinearImages ) where

import Control.Monad

import System.IO

import Data.Word

import qualified Data.ByteString as BS
import Data.Binary.Strict.Get

import Data.Array.Repa as R
import Data.Array.Repa.Repr.ByteString as R

readLabels :: FilePath -> IO (Array B DIM1 Word8)
readLabels p = withFile p ReadMode $ \h -> do
  header <- BS.hGet h 8
  let (Right (magic, nElems), _) = runGet (liftM2 (,) getWord32be getWord32be) header
      shape = Z :. fromIntegral nElems
  when (magic /= 2049) $ error "MNIST format error: bad magic number"
  fromByteString shape `liftM` BS.hGetContents h

-- TODO: factor common parts out from two functions below:
  
-- ^ read matrix - array of images. while in input format every image is represented by matrix,
--   we convert it into just one row - for ML purposes pixel order is insignificant
readLinearImages :: FilePath -> IO (Array B DIM2 Word8)
readLinearImages p = withFile p ReadMode $ \h -> do
  header <- BS.hGet h 16
  let (Right (magic, nElems, ir, ic), _) =
        runGet (liftM4 (,,,) getWord32be getWord32be getWord32be getWord32be) header
  when (magic /= 2051) $ error "MNIST format error: bad magic number"
  let rows = fromIntegral nElems
      cols = fromIntegral $ ir * ic
      shape = (Z :. rows :. cols)
  fromByteString shape `liftM` BS.hGetContents h

-- ^ the same as above but read images into matrices, resulting in 3d matrix
readImages :: FilePath -> IO (Array B DIM3 Word8)
readImages p = withFile p ReadMode $ \h -> do
  header <- BS.hGet h 16
  let (Right (magic, nElems, rows, cols), _) =
        runGet (liftM4 (,,,) getWord32be getWord32be getWord32be getWord32be) header
  when (magic /= 2051) $ error "MNIST format error: bad magic number"
  let shape = (Z :. fromIntegral nElems :. fromIntegral rows :. fromIntegral cols)
  fromByteString shape `liftM` BS.hGetContents h

    
            