{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module:      Data.RTCM3
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- RTCMv3 message containers.

module Data.RTCM3
  ( RTCM3Msg (..)
  , module Export
  ) where

import BasicPrelude
import Control.Lens            hiding ((.=))
import Data.Aeson              hiding (decode)
import Data.Aeson.Lens
import Data.Binary
import Data.ByteString.Lazy
import Data.RTCM3.Antennas     as Export
import Data.RTCM3.Ephemerides  as Export
import Data.RTCM3.Observations as Export
import Data.RTCM3.SSR          as Export
import Data.RTCM3.System       as Export
import Data.RTCM3.Types        as Export

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
   | RTCM3Msg1019    Msg1019 Msg
   | RTCM3Msg1020    Msg1020 Msg
   | RTCM3Msg1033    Msg1033 Msg
   | RTCM3Msg1057    Msg1057 Msg
   | RTCM3Msg1058    Msg1058 Msg
   | RTCM3Msg1063    Msg1063 Msg
   | RTCM3Msg1064    Msg1064 Msg
   | RTCM3Msg1230    Msg1230 Msg
   | RTCM3MsgUnknown Word16  Msg
   | RTCM3MsgBadCrc          Msg
   | RTCM3MsgEmpty           Msg
   deriving ( Show, Read, Eq )

$(makePrisms ''RTCM3Msg)

instance Binary RTCM3Msg where
  get = do
    preamble <- getWord8
    if preamble /= msgRTCM3Preamble then get else
      decoder <$> get where
        decoder m@Msg {..}
          | crc /= _msgRTCM3Crc = RTCM3MsgBadCrc m
          | 0 == _msgRTCM3Len = RTCM3MsgEmpty m
          | num == msg1001 = RTCM3Msg1001 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1002 = RTCM3Msg1002 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1003 = RTCM3Msg1003 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1004 = RTCM3Msg1004 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1005 = RTCM3Msg1005 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1006 = RTCM3Msg1006 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1007 = RTCM3Msg1007 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1008 = RTCM3Msg1008 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1009 = RTCM3Msg1009 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1010 = RTCM3Msg1010 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1011 = RTCM3Msg1011 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1012 = RTCM3Msg1012 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1013 = RTCM3Msg1013 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1019 = RTCM3Msg1019 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1020 = RTCM3Msg1020 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1033 = RTCM3Msg1033 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1057 = RTCM3Msg1057 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1058 = RTCM3Msg1058 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1063 = RTCM3Msg1063 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1064 = RTCM3Msg1064 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1230 = RTCM3Msg1230 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | otherwise = RTCM3MsgUnknown num m where
            crc = checkCrc _msgRTCM3Len $ unBytes _msgRTCM3Payload
            num = checkNum $ unBytes _msgRTCM3Payload

  put rm = do
    putWord8 msgRTCM3Preamble
    encoder rm where
      encoder (RTCM3Msg1001    _n m) = put m
      encoder (RTCM3Msg1002    _n m) = put m
      encoder (RTCM3Msg1003    _n m) = put m
      encoder (RTCM3Msg1004    _n m) = put m
      encoder (RTCM3Msg1005    _n m) = put m
      encoder (RTCM3Msg1006    _n m) = put m
      encoder (RTCM3Msg1007    _n m) = put m
      encoder (RTCM3Msg1008    _n m) = put m
      encoder (RTCM3Msg1009    _n m) = put m
      encoder (RTCM3Msg1010    _n m) = put m
      encoder (RTCM3Msg1011    _n m) = put m
      encoder (RTCM3Msg1012    _n m) = put m
      encoder (RTCM3Msg1013    _n m) = put m
      encoder (RTCM3Msg1019    _n m) = put m
      encoder (RTCM3Msg1020    _n m) = put m
      encoder (RTCM3Msg1033    _n m) = put m
      encoder (RTCM3Msg1057    _n m) = put m
      encoder (RTCM3Msg1058    _n m) = put m
      encoder (RTCM3Msg1063    _n m) = put m
      encoder (RTCM3Msg1064    _n m) = put m
      encoder (RTCM3Msg1230    _n m) = put m
      encoder (RTCM3MsgUnknown _n m) = put m
      encoder (RTCM3MsgBadCrc     m) = put m
      encoder (RTCM3MsgEmpty      m) = put m

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
  msg f (RTCM3Msg1019    n m) = RTCM3Msg1019    n <$> f m
  msg f (RTCM3Msg1020    n m) = RTCM3Msg1020    n <$> f m
  msg f (RTCM3Msg1033    n m) = RTCM3Msg1033    n <$> f m
  msg f (RTCM3Msg1057    n m) = RTCM3Msg1057    n <$> f m
  msg f (RTCM3Msg1058    n m) = RTCM3Msg1058    n <$> f m
  msg f (RTCM3Msg1063    n m) = RTCM3Msg1063    n <$> f m
  msg f (RTCM3Msg1064    n m) = RTCM3Msg1064    n <$> f m
  msg f (RTCM3Msg1230    n m) = RTCM3Msg1230    n <$> f m
  msg f (RTCM3MsgUnknown n m) = RTCM3MsgUnknown n <$> f m
  msg f (RTCM3MsgBadCrc    m) = RTCM3MsgBadCrc    <$> f m
  msg f (RTCM3MsgEmpty     m) = RTCM3MsgEmpty     <$> f m

(<<>>) :: Value -> Value -> Value
(<<>>) a b = fromMaybe Null $ do
  c <- preview _Object a
  d <- preview _Object b
  return $ review _Object $ c <> d

instance ToJSON RTCM3Msg where
  toJSON (RTCM3Msg1001    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1002    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1003    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1004    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1005    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1006    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1007    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1008    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1009    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1010    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1011    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1012    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1013    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1019    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1020    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1033    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1057    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1058    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1063    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1064    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1230    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3MsgUnknown n m) = object [ "num" .= n ] <<>> toJSON m
  toJSON (RTCM3MsgBadCrc    m) = toJSON m
  toJSON (RTCM3MsgEmpty     m) = toJSON m

instance FromJSON RTCM3Msg where
  parseJSON obj@(Object o) = do
    payload <- o .: "payload"
    decoder (checkNum $ unBytes payload) payload where
      decoder num payload
        | num == msg1001 = RTCM3Msg1001 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1002 = RTCM3Msg1002 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1003 = RTCM3Msg1003 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1004 = RTCM3Msg1004 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1005 = RTCM3Msg1005 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1006 = RTCM3Msg1006 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1007 = RTCM3Msg1007 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1008 = RTCM3Msg1008 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1009 = RTCM3Msg1009 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1010 = RTCM3Msg1010 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1011 = RTCM3Msg1011 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1012 = RTCM3Msg1012 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1013 = RTCM3Msg1013 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1019 = RTCM3Msg1019 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1020 = RTCM3Msg1020 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1033 = RTCM3Msg1033 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1057 = RTCM3Msg1057 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1058 = RTCM3Msg1058 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1063 = RTCM3Msg1063 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1064 = RTCM3Msg1064 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1230 = RTCM3Msg1230 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | otherwise = RTCM3MsgUnknown <$> pure num <*> parseJSON obj
  parseJSON _ = mzero
