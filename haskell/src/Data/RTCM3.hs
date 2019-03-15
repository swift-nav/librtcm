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
  ( module Data.RTCM3
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
import Data.RTCM3.MSM          as Export
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
   | RTCM3Msg1029    Msg1029 Msg
   | RTCM3Msg1033    Msg1033 Msg
   | RTCM3Msg1045    Msg1045 Msg
   | RTCM3Msg1046    Msg1046 Msg
   | RTCM3Msg1057    Msg1057 Msg
   | RTCM3Msg1058    Msg1058 Msg
   | RTCM3Msg1059    Msg1059 Msg
   | RTCM3Msg1060    Msg1060 Msg
   | RTCM3Msg1063    Msg1063 Msg
   | RTCM3Msg1064    Msg1064 Msg
   | RTCM3Msg1065    Msg1065 Msg
   | RTCM3Msg1066    Msg1066 Msg
   | RTCM3Msg1074    Msg1074 Msg
   | RTCM3Msg1075    Msg1075 Msg
   | RTCM3Msg1076    Msg1076 Msg
   | RTCM3Msg1077    Msg1077 Msg
   | RTCM3Msg1084    Msg1084 Msg
   | RTCM3Msg1085    Msg1085 Msg
   | RTCM3Msg1086    Msg1086 Msg
   | RTCM3Msg1087    Msg1087 Msg
   | RTCM3Msg1094    Msg1094 Msg
   | RTCM3Msg1095    Msg1095 Msg
   | RTCM3Msg1096    Msg1096 Msg
   | RTCM3Msg1097    Msg1097 Msg
   | RTCM3Msg1104    Msg1104 Msg
   | RTCM3Msg1105    Msg1105 Msg
   | RTCM3Msg1106    Msg1106 Msg
   | RTCM3Msg1107    Msg1107 Msg
   | RTCM3Msg1114    Msg1114 Msg
   | RTCM3Msg1115    Msg1115 Msg
   | RTCM3Msg1116    Msg1116 Msg
   | RTCM3Msg1117    Msg1117 Msg
   | RTCM3Msg1124    Msg1124 Msg
   | RTCM3Msg1125    Msg1125 Msg
   | RTCM3Msg1126    Msg1126 Msg
   | RTCM3Msg1127    Msg1127 Msg
   | RTCM3Msg1230    Msg1230 Msg
   | RTCM3Msg1265    Msg1265 Msg
   | RTCM3Msg1266    Msg1266 Msg
   | RTCM3MsgUnknown         Msg
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
          | num == msg1029 = RTCM3Msg1029 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1033 = RTCM3Msg1033 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1045 = RTCM3Msg1045 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1046 = RTCM3Msg1046 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1057 = RTCM3Msg1057 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1058 = RTCM3Msg1058 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1059 = RTCM3Msg1059 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1060 = RTCM3Msg1060 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1063 = RTCM3Msg1063 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1064 = RTCM3Msg1064 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1065 = RTCM3Msg1065 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1066 = RTCM3Msg1066 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1074 = RTCM3Msg1074 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1075 = RTCM3Msg1075 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1076 = RTCM3Msg1076 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1077 = RTCM3Msg1077 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1084 = RTCM3Msg1084 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1085 = RTCM3Msg1085 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1086 = RTCM3Msg1086 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1087 = RTCM3Msg1087 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1094 = RTCM3Msg1094 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1095 = RTCM3Msg1095 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1096 = RTCM3Msg1096 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1097 = RTCM3Msg1097 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1104 = RTCM3Msg1104 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1105 = RTCM3Msg1105 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1106 = RTCM3Msg1106 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1107 = RTCM3Msg1107 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1114 = RTCM3Msg1114 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1115 = RTCM3Msg1125 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1116 = RTCM3Msg1116 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1117 = RTCM3Msg1117 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1124 = RTCM3Msg1124 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1125 = RTCM3Msg1125 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1126 = RTCM3Msg1126 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1127 = RTCM3Msg1127 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1230 = RTCM3Msg1230 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1265 = RTCM3Msg1265 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | num == msg1266 = RTCM3Msg1266 (decode $ fromStrict $ unBytes _msgRTCM3Payload) m
          | otherwise = RTCM3MsgUnknown m where
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
      encoder (RTCM3Msg1029    _n m) = put m
      encoder (RTCM3Msg1033    _n m) = put m
      encoder (RTCM3Msg1045    _n m) = put m
      encoder (RTCM3Msg1046    _n m) = put m
      encoder (RTCM3Msg1057    _n m) = put m
      encoder (RTCM3Msg1058    _n m) = put m
      encoder (RTCM3Msg1059    _n m) = put m
      encoder (RTCM3Msg1060    _n m) = put m
      encoder (RTCM3Msg1063    _n m) = put m
      encoder (RTCM3Msg1064    _n m) = put m
      encoder (RTCM3Msg1065    _n m) = put m
      encoder (RTCM3Msg1066    _n m) = put m
      encoder (RTCM3Msg1074    _n m) = put m
      encoder (RTCM3Msg1075    _n m) = put m
      encoder (RTCM3Msg1076    _n m) = put m
      encoder (RTCM3Msg1077    _n m) = put m
      encoder (RTCM3Msg1084    _n m) = put m
      encoder (RTCM3Msg1085    _n m) = put m
      encoder (RTCM3Msg1086    _n m) = put m
      encoder (RTCM3Msg1087    _n m) = put m
      encoder (RTCM3Msg1094    _n m) = put m
      encoder (RTCM3Msg1095    _n m) = put m
      encoder (RTCM3Msg1096    _n m) = put m
      encoder (RTCM3Msg1097    _n m) = put m
      encoder (RTCM3Msg1104    _n m) = put m
      encoder (RTCM3Msg1105    _n m) = put m
      encoder (RTCM3Msg1106    _n m) = put m
      encoder (RTCM3Msg1107    _n m) = put m
      encoder (RTCM3Msg1114    _n m) = put m
      encoder (RTCM3Msg1115    _n m) = put m
      encoder (RTCM3Msg1116    _n m) = put m
      encoder (RTCM3Msg1117    _n m) = put m
      encoder (RTCM3Msg1124    _n m) = put m
      encoder (RTCM3Msg1125    _n m) = put m
      encoder (RTCM3Msg1126    _n m) = put m
      encoder (RTCM3Msg1127    _n m) = put m
      encoder (RTCM3Msg1230    _n m) = put m
      encoder (RTCM3Msg1265    _n m) = put m
      encoder (RTCM3Msg1266    _n m) = put m
      encoder (RTCM3MsgUnknown    m) = put m
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
  msg f (RTCM3Msg1029    n m) = RTCM3Msg1029    n <$> f m
  msg f (RTCM3Msg1033    n m) = RTCM3Msg1033    n <$> f m
  msg f (RTCM3Msg1045    n m) = RTCM3Msg1045    n <$> f m
  msg f (RTCM3Msg1046    n m) = RTCM3Msg1046    n <$> f m
  msg f (RTCM3Msg1057    n m) = RTCM3Msg1057    n <$> f m
  msg f (RTCM3Msg1058    n m) = RTCM3Msg1058    n <$> f m
  msg f (RTCM3Msg1059    n m) = RTCM3Msg1059    n <$> f m
  msg f (RTCM3Msg1060    n m) = RTCM3Msg1060    n <$> f m
  msg f (RTCM3Msg1063    n m) = RTCM3Msg1063    n <$> f m
  msg f (RTCM3Msg1064    n m) = RTCM3Msg1064    n <$> f m
  msg f (RTCM3Msg1065    n m) = RTCM3Msg1065    n <$> f m
  msg f (RTCM3Msg1066    n m) = RTCM3Msg1066    n <$> f m
  msg f (RTCM3Msg1074    n m) = RTCM3Msg1074    n <$> f m
  msg f (RTCM3Msg1075    n m) = RTCM3Msg1075    n <$> f m
  msg f (RTCM3Msg1076    n m) = RTCM3Msg1076    n <$> f m
  msg f (RTCM3Msg1077    n m) = RTCM3Msg1077    n <$> f m
  msg f (RTCM3Msg1084    n m) = RTCM3Msg1084    n <$> f m
  msg f (RTCM3Msg1085    n m) = RTCM3Msg1085    n <$> f m
  msg f (RTCM3Msg1086    n m) = RTCM3Msg1086    n <$> f m
  msg f (RTCM3Msg1087    n m) = RTCM3Msg1087    n <$> f m
  msg f (RTCM3Msg1094    n m) = RTCM3Msg1094    n <$> f m
  msg f (RTCM3Msg1095    n m) = RTCM3Msg1095    n <$> f m
  msg f (RTCM3Msg1096    n m) = RTCM3Msg1096    n <$> f m
  msg f (RTCM3Msg1097    n m) = RTCM3Msg1097    n <$> f m
  msg f (RTCM3Msg1104    n m) = RTCM3Msg1104    n <$> f m
  msg f (RTCM3Msg1105    n m) = RTCM3Msg1105    n <$> f m
  msg f (RTCM3Msg1106    n m) = RTCM3Msg1106    n <$> f m
  msg f (RTCM3Msg1107    n m) = RTCM3Msg1107    n <$> f m
  msg f (RTCM3Msg1114    n m) = RTCM3Msg1114    n <$> f m
  msg f (RTCM3Msg1115    n m) = RTCM3Msg1115    n <$> f m
  msg f (RTCM3Msg1116    n m) = RTCM3Msg1116    n <$> f m
  msg f (RTCM3Msg1117    n m) = RTCM3Msg1117    n <$> f m
  msg f (RTCM3Msg1124    n m) = RTCM3Msg1124    n <$> f m
  msg f (RTCM3Msg1125    n m) = RTCM3Msg1125    n <$> f m
  msg f (RTCM3Msg1126    n m) = RTCM3Msg1126    n <$> f m
  msg f (RTCM3Msg1127    n m) = RTCM3Msg1127    n <$> f m
  msg f (RTCM3Msg1230    n m) = RTCM3Msg1230    n <$> f m
  msg f (RTCM3Msg1265    n m) = RTCM3Msg1265    n <$> f m
  msg f (RTCM3Msg1266    n m) = RTCM3Msg1266    n <$> f m
  msg f (RTCM3MsgUnknown   m) = RTCM3MsgUnknown   <$> f m
  msg f (RTCM3MsgBadCrc    m) = RTCM3MsgBadCrc    <$> f m
  msg f (RTCM3MsgEmpty     m) = RTCM3MsgEmpty     <$> f m

(<<>>) :: Value -> Value -> Value
(<<>>) a b = fromMaybe Null $ do
  c <- preview _Object a
  d <- preview _Object b
  pure $ review _Object $ c <> d

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
  toJSON (RTCM3Msg1029    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1033    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1045    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1046    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1057    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1058    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1059    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1060    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1063    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1064    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1065    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1066    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1074    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1075    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1076    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1077    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1084    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1085    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1086    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1087    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1094    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1095    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1096    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1097    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1104    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1105    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1106    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1107    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1114    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1115    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1116    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1117    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1124    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1125    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1126    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1127    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1230    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1265    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3Msg1266    n m) = toJSON n <<>> toJSON m
  toJSON (RTCM3MsgUnknown   m) = toJSON m
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
        | num == msg1029 = RTCM3Msg1029 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1033 = RTCM3Msg1033 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1045 = RTCM3Msg1045 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1046 = RTCM3Msg1046 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1057 = RTCM3Msg1057 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1058 = RTCM3Msg1058 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1059 = RTCM3Msg1059 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1060 = RTCM3Msg1060 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1063 = RTCM3Msg1063 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1064 = RTCM3Msg1064 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1065 = RTCM3Msg1065 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1066 = RTCM3Msg1066 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1074 = RTCM3Msg1074 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1230 = RTCM3Msg1230 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | num == msg1265 = RTCM3Msg1265 <$> pure (decode $ fromStrict $ unBytes payload) <*> parseJSON obj
        | otherwise = RTCM3MsgUnknown <$> parseJSON obj
  parseJSON _ = mzero
