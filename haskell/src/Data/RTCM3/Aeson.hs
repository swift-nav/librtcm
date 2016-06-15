{-# OPTIONS -fno-warn-orphans #-}
-- |
-- Module:      Data.RTCM3.Extras
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Mark Fine <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- Extra stuff.

module Data.RTCM3.Aeson where

import BasicPrelude
import Data.Aeson
import Data.ByteString.Base64   as Base64
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Word.Word24

-- ByteString doesn't have Aeson instances defined for it since
-- arbitrary ByteString's aren't really valid JSON. This defines
-- orphaned instances for ByteStrings that are expected to be valid
-- text.

instance ToJSON ByteString where
  toJSON = toJSON . decodeUtf8With ignore . Base64.encode

instance FromJSON ByteString where
  parseJSON = withText "ByteString" (pure . Base64.decodeLenient . encodeUtf8)

instance ToJSON Word24 where
  toJSON = Number . fromIntegral

instance FromJSON Word24 where
  parseJSON = withScientific "Word24" (pure . truncate)
