-- |
-- Module:      Data.RTCM3.Extras
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Mark Fine <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- Extra stuff.

module Data.RTCM3.Extras
  ( getInt8
  , getInt16be
  , getInt32be
  , getInt64be
  , putInt8
  , putInt16be
  , putInt32be
  , putInt64be
  , getWord24be
  , putWord24be
  ) where

import           BasicPrelude
import           Data.Binary
import qualified Data.Binary.Bits.Get as B
import qualified Data.Binary.Bits.Put as B
import           Data.Bits
import           Data.Int
import           Data.Word.Word24

signExtend8 :: Int -> Word8 -> Int8
signExtend8 n x = (fromIntegral x `shiftL` (8 - n)) `shiftR` (8 - n)

signExtend16 :: Int -> Word16 -> Int16
signExtend16 n x = (fromIntegral x `shiftL` (16 - n)) `shiftR` (16 - n)

signExtend32 :: Int -> Word32 -> Int32
signExtend32 n x = (fromIntegral x `shiftL` (32 - n)) `shiftR` (32 - n)

signExtend64 :: Int -> Word64 -> Int64
signExtend64 n x = (fromIntegral x `shiftL` (64 - n)) `shiftR` (64 - n)

getInt8 :: Int -> B.BitGet Int8
getInt8 n = signExtend8 n <$> B.getWord8 n

getInt16be :: Int -> B.BitGet Int16
getInt16be n = signExtend16 n <$> B.getWord16be n

getInt32be :: Int -> B.BitGet Int32
getInt32be n = signExtend32 n <$> B.getWord32be n

getInt64be :: Int -> B.BitGet Int64
getInt64be n = signExtend64 n <$> B.getWord64be n

putInt8 :: Int -> Int8 -> B.BitPut ()
putInt8 n = B.putWord8 n . fromIntegral

putInt16be :: Int -> Int16 -> B.BitPut ()
putInt16be n = B.putWord16be n . fromIntegral

putInt32be :: Int -> Int32 -> B.BitPut ()
putInt32be n = B.putWord32be n . fromIntegral

putInt64be :: Int -> Int64 -> B.BitPut ()
putInt64be n = B.putWord64be n . fromIntegral

getWord24be :: Get Word24
getWord24be = do
    b1 <- fromIntegral <$> getWord8
    b2 <- fromIntegral <$> getWord8
    b3 <- fromIntegral <$> getWord8
    return $ fromInteger $ shiftL b1 16 .|. shiftL b2 8 .|. b3
{-# INLINE getWord24be #-}

putWord24be :: Word24 -> Put
putWord24be w = do
    putWord8 $ fromIntegral $ shiftR w 16
    putWord8 $ fromIntegral $ shiftR w 8
    putWord8 $ fromIntegral w
{-# INLINE putWord24be #-}
