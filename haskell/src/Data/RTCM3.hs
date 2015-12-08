-- |
-- Module:      Data.RTCM3
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark.fine@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- RTCMv3 message containers.

module Data.RTCM3
  ( RTCM3Msg (..)
  , module Data.RTCM3.Antennas
  , module Data.RTCM3.Observations
  , module Data.RTCM3.Types
  ) where

import BasicPrelude
import Data.Binary
import Data.ByteString.Lazy
import Data.RTCM3.Antennas
import Data.RTCM3.Observations
import Data.RTCM3.Types

-- | An RTCM message ADT composed of all defined RTCM messages.
--
-- Includes RTCMMsgUnknown for valid RTCM messages with undefined message
-- types and RTCMMsgBadCRC for RTCM messages with invalid CRC checksums.
data RTCM3Msg =
     RTCM3Msg1001 Msg1001 Msg
   | RTCM3Msg1002 Msg1002 Msg
   | RTCM3Msg1003 Msg1003 Msg
   | RTCM3Msg1004 Msg1004 Msg
   | RTCM3Msg1005 Msg1005 Msg
   | RTCM3Msg1006 Msg1006 Msg
   | RTCM3Msg1007 Msg1007 Msg
   | RTCM3Msg1008 Msg1008 Msg
   | RTCM3MsgUnknown Word16 Msg
   | RTCM3MsgBadCrc Msg
   deriving ( Show, Read, Eq )

instance Binary RTCM3Msg where
  get = do
    preamble <- getWord8
    if preamble /= msgRTCM3Preamble then get else do
      rtcm3 <- get
      return $ decode' rtcm3  where
        decode' rtcm3@Msg {..}
          | checkCrc _msgRTCM3Len _msgRTCM3Payload /= _msgRTCM3Crc = RTCM3MsgBadCrc rtcm3
          | num == msg1001 = RTCM3Msg1001 (decode $ fromStrict _msgRTCM3Payload) rtcm3
          | num == msg1002 = RTCM3Msg1002 (decode $ fromStrict _msgRTCM3Payload) rtcm3
          | num == msg1003 = RTCM3Msg1003 (decode $ fromStrict _msgRTCM3Payload) rtcm3
          | num == msg1004 = RTCM3Msg1004 (decode $ fromStrict _msgRTCM3Payload) rtcm3
          | num == msg1005 = RTCM3Msg1005 (decode $ fromStrict _msgRTCM3Payload) rtcm3
          | num == msg1006 = RTCM3Msg1006 (decode $ fromStrict _msgRTCM3Payload) rtcm3
          | num == msg1007 = RTCM3Msg1007 (decode $ fromStrict _msgRTCM3Payload) rtcm3
          | num == msg1008 = RTCM3Msg1008 (decode $ fromStrict _msgRTCM3Payload) rtcm3
          | otherwise = RTCM3MsgUnknown num rtcm3 where
            num = checkNum _msgRTCM3Payload

  put msg = do
    putWord8 msgRTCM3Preamble
    encode' msg where
      encode' (RTCM3Msg1001 _msg rtcm3) = put rtcm3
      encode' (RTCM3Msg1002 _msg rtcm3) = put rtcm3
      encode' (RTCM3Msg1003 _msg rtcm3) = put rtcm3
      encode' (RTCM3Msg1004 _msg rtcm3) = put rtcm3
      encode' (RTCM3Msg1005 _msg rtcm3) = put rtcm3
      encode' (RTCM3Msg1006 _msg rtcm3) = put rtcm3
      encode' (RTCM3Msg1007 _msg rtcm3) = put rtcm3
      encode' (RTCM3Msg1008 _msg rtcm3) = put rtcm3
      encode' (RTCM3MsgUnknown _num rtcm3) = put rtcm3
      encode' (RTCM3MsgBadCrc rtcm3) = put rtcm3
