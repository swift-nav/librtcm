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
  , module Data.RTCM3.SSR
  , module Data.RTCM3.System
  , module Data.RTCM3.Types
  ) where

import BasicPrelude
import Control.Lens
import Data.Binary
import Data.ByteString.Lazy
import Data.RTCM3.Antennas
import Data.RTCM3.Observations
import Data.RTCM3.SSR
import Data.RTCM3.System
import Data.RTCM3.Types

-- | An RTCM message ADT composed of all defined RTCM messages.
--
-- Includes RTCMMsgUnknown for valid RTCM messages with undefined message
-- types and RTCMMsgBadCRC for RTCM messages with invalid CRC checksums.
data RTCM3Msg =
     RTCM3Msg1001    Msg1001 Msg
   | RTCM3Msg1002    Msg1002 Msg
   | RTCM3Msg1003    Msg1003 Msg
   | RTCM3Msg1004    Msg1004 Msg
   | RTCM3Msg1005    Msg1005 Msg
   | RTCM3Msg1006    Msg1006 Msg
   | RTCM3Msg1007    Msg1007 Msg
   | RTCM3Msg1008    Msg1008 Msg
   | RTCM3Msg1009    Msg1009 Msg
   | RTCM3Msg1010    Msg1010 Msg
   | RTCM3Msg1011    Msg1011 Msg
   | RTCM3Msg1012    Msg1012 Msg
   | RTCM3Msg1013    Msg1013 Msg
   | RTCM3Msg1033    Msg1033 Msg
   | RTCM3Msg1057    Msg1057 Msg
   | RTCM3Msg1058    Msg1058 Msg
   | RTCM3Msg1230    Msg1230 Msg
   | RTCM3MsgUnknown         Msg
   | RTCM3MsgBadCrc          Msg
   deriving ( Show, Read, Eq )

$(makePrisms ''RTCM3Msg)

instance Binary RTCM3Msg where
  get = do
    preamble <- getWord8
    if preamble /= msgRTCM3Preamble then get else do
      decode' <$> get where
        decode' m@Msg {..}
          | crc /= _msgRTCM3Crc = RTCM3MsgBadCrc m
          | num == msg1001 = RTCM3Msg1001 (decode $ fromStrict _msgRTCM3Payload) m
          | num == msg1002 = RTCM3Msg1002 (decode $ fromStrict _msgRTCM3Payload) m
          | num == msg1003 = RTCM3Msg1003 (decode $ fromStrict _msgRTCM3Payload) m
          | num == msg1004 = RTCM3Msg1004 (decode $ fromStrict _msgRTCM3Payload) m
          | num == msg1005 = RTCM3Msg1005 (decode $ fromStrict _msgRTCM3Payload) m
          | num == msg1006 = RTCM3Msg1006 (decode $ fromStrict _msgRTCM3Payload) m
          | num == msg1007 = RTCM3Msg1007 (decode $ fromStrict _msgRTCM3Payload) m
          | num == msg1008 = RTCM3Msg1008 (decode $ fromStrict _msgRTCM3Payload) m
          | num == msg1009 = RTCM3Msg1009 (decode $ fromStrict _msgRTCM3Payload) m
          | num == msg1010 = RTCM3Msg1010 (decode $ fromStrict _msgRTCM3Payload) m
          | num == msg1011 = RTCM3Msg1011 (decode $ fromStrict _msgRTCM3Payload) m
          | num == msg1012 = RTCM3Msg1012 (decode $ fromStrict _msgRTCM3Payload) m
          | num == msg1013 = RTCM3Msg1013 (decode $ fromStrict _msgRTCM3Payload) m
          | num == msg1033 = RTCM3Msg1033 (decode $ fromStrict _msgRTCM3Payload) m
          | num == msg1057 = RTCM3Msg1057 (decode $ fromStrict _msgRTCM3Payload) m
          | num == msg1230 = RTCM3Msg1230 (decode $ fromStrict _msgRTCM3Payload) m
          | otherwise = RTCM3MsgUnknown m where
            crc = checkCrc _msgRTCM3Len _msgRTCM3Payload
            num = checkNum _msgRTCM3Payload

  put rm = do
    putWord8 msgRTCM3Preamble
    encode' rm where
      encode' (RTCM3Msg1001    _n m) = put m
      encode' (RTCM3Msg1002    _n m) = put m
      encode' (RTCM3Msg1003    _n m) = put m
      encode' (RTCM3Msg1004    _n m) = put m
      encode' (RTCM3Msg1005    _n m) = put m
      encode' (RTCM3Msg1006    _n m) = put m
      encode' (RTCM3Msg1007    _n m) = put m
      encode' (RTCM3Msg1008    _n m) = put m
      encode' (RTCM3Msg1009    _n m) = put m
      encode' (RTCM3Msg1010    _n m) = put m
      encode' (RTCM3Msg1011    _n m) = put m
      encode' (RTCM3Msg1012    _n m) = put m
      encode' (RTCM3Msg1013    _n m) = put m
      encode' (RTCM3Msg1033    _n m) = put m
      encode' (RTCM3Msg1057    _n m) = put m
      encode' (RTCM3Msg1058    _n m) = put m
      encode' (RTCM3Msg1230    _n m) = put m
      encode' (RTCM3MsgUnknown    m) = put m
      encode' (RTCM3MsgBadCrc     m) = put m

instance HasMsg RTCM3Msg where
  msg f (RTCM3Msg1001    n m) = RTCM3Msg1001    n <$> f m
  msg f (RTCM3Msg1002    n m) = RTCM3Msg1002    n <$> f m
  msg f (RTCM3Msg1003    n m) = RTCM3Msg1003    n <$> f m
  msg f (RTCM3Msg1004    n m) = RTCM3Msg1004    n <$> f m
  msg f (RTCM3Msg1005    n m) = RTCM3Msg1005    n <$> f m
  msg f (RTCM3Msg1006    n m) = RTCM3Msg1006    n <$> f m
  msg f (RTCM3Msg1007    n m) = RTCM3Msg1007    n <$> f m
  msg f (RTCM3Msg1008    n m) = RTCM3Msg1008    n <$> f m
  msg f (RTCM3Msg1009    n m) = RTCM3Msg1009    n <$> f m
  msg f (RTCM3Msg1010    n m) = RTCM3Msg1010    n <$> f m
  msg f (RTCM3Msg1011    n m) = RTCM3Msg1011    n <$> f m
  msg f (RTCM3Msg1012    n m) = RTCM3Msg1012    n <$> f m
  msg f (RTCM3Msg1013    n m) = RTCM3Msg1013    n <$> f m
  msg f (RTCM3Msg1033    n m) = RTCM3Msg1033    n <$> f m
  msg f (RTCM3Msg1057    n m) = RTCM3Msg1057    n <$> f m
  msg f (RTCM3Msg1058    n m) = RTCM3Msg1058    n <$> f m
  msg f (RTCM3Msg1230    n m) = RTCM3Msg1230    n <$> f m
  msg f (RTCM3MsgUnknown   m) = RTCM3MsgUnknown   <$> f m
  msg f (RTCM3MsgBadCrc    m) = RTCM3MsgBadCrc    <$> f m

