{-# OPTIONS -fno-warn-orphans  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module:      Data.RTCM3.Types
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- Common RTCMv3 type requirements, containers, and serialization
-- utilities.

module Data.RTCM3.Types where

import           BasicPrelude
import           Control.Lens             hiding ((.=))
import           Data.Aeson
import           Data.Binary
import qualified Data.Binary.Bits.Get     as B
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString.Base64   as Base64
import           Data.ByteString.Builder
import           Data.ByteString.Lazy     hiding (ByteString)
import           Data.CRC24Q
import           Data.RTCM3.Extras
import           Data.Text.Encoding
import           Data.Text.Encoding.Error
import           Data.Word.Word24

msgRTCM3Preamble :: Word8
msgRTCM3Preamble = 0xD3

newtype Bytes = Bytes
  { unBytes :: ByteString
  } deriving ( Show, Read, Eq )

data Msg = Msg
  { _msgRTCM3Len     :: !Word16
  , _msgRTCM3Payload :: !Bytes
  , _msgRTCM3Crc     :: !Word24
  } deriving ( Show, Read, Eq )

$(makeClassy ''Msg)

instance ToJSON Bytes where
  toJSON = toJSON . decodeUtf8With ignore . Base64.encode . unBytes

instance ToJSON Word24 where
  toJSON = Number . fromIntegral

instance ToJSON Msg where
  toJSON Msg {..} = object
    [ "len"     .= _msgRTCM3Len
    , "payload" .= _msgRTCM3Payload
    , "crc"     .= _msgRTCM3Crc
    ]

instance FromJSON Bytes where
  parseJSON = withText "ByteString" (pure . Bytes . Base64.decodeLenient . encodeUtf8)

instance FromJSON Word24 where
  parseJSON = withScientific "Word24" (pure . truncate)

instance FromJSON Msg where
  parseJSON (Object v) =
    Msg <$> v .: "len"
        <*> v .: "payload"
        <*> v .: "crc"
  parseJSON _ = mzero

instance Binary Msg where
  get = do
    _msgRTCM3Len     <- getWord16be
    _msgRTCM3Payload <- fmap Bytes $ getByteString $ fromIntegral _msgRTCM3Len
    _msgRTCM3Crc     <- getWord24be
    return Msg {..}

  put Msg {..} = do
    putWord16be _msgRTCM3Len
    putByteString $ unBytes _msgRTCM3Payload
    putWord24be _msgRTCM3Crc

checkCrc :: Word16 -> ByteString -> Word24
checkCrc len payload =
  crc24q $ toLazyByteString $
    word8 msgRTCM3Preamble  <>
    word16BE len            <>
    byteString payload

checkNum :: ByteString -> Word16
checkNum payload =
  flip runGet (fromStrict payload) $ B.runBitGet $
    B.getWord16be 12

msgRTCM3Num :: HasMsg m => m -> Word16
msgRTCM3Num = checkNum . unBytes . (^. msgRTCM3Payload)

class Binary a => ToRTCM3 a where
  toRTCM3 :: a -> Msg
