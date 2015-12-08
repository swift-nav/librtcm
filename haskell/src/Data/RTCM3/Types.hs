-- |
-- Module:      Data.RTCM3.Types
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Mark Fine <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- Common RTCMv3 type requirements, containers, and serialization
-- utilities.

module Data.RTCM3.Types where

import           BasicPrelude
import           Control.Lens
import           Data.Binary
import qualified Data.Binary.Bits.Get as B
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString.Builder
import           Data.ByteString.Lazy hiding ( ByteString )
import           Data.CRC24Q
import           Data.RTCM3.Extras
import           Data.Word.Word24

msgRTCM3Preamble :: Word8
msgRTCM3Preamble = 0xD3

data Msg = Msg
  { _msgRTCM3Len     :: Word16
  , _msgRTCM3Payload :: !ByteString
  , _msgRTCM3Crc     :: Word24
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg)

instance Binary Msg where
  get = do
    _msgRTCM3Len     <- getWord16be
    _msgRTCM3Payload <- getByteString $ fromIntegral _msgRTCM3Len
    _msgRTCM3Crc     <- getWord24be
    return Msg {..}

  put Msg {..} = do
    putWord16be _msgRTCM3Len
    putByteString _msgRTCM3Payload
    putWord24be _msgRTCM3Crc

checkNum :: ByteString -> Word16
checkNum payload =
  flip runGet (fromStrict payload) $ B.runBitGet $
    B.getWord16be 12

checkCrc :: Word16 -> ByteString -> Word24
checkCrc len payload =
  crc24q $ toLazyByteString $
    word8 msgRTCM3Preamble  <>
    word16BE len            <>
    byteString payload

class Binary a => ToRTCM3 a where
  toRTCM3 :: a -> Msg
