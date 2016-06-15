-- |
-- Module:      Main
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Mark Fine <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- RTCM3 to JSON tool - reads RTCM3 binary from stdin and sends RTCM3 JSON
-- to stdout.

import           BasicPrelude
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.Encode
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BL
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import           Data.Conduit.Serialization.Binary
import           Data.RTCM3
import           System.IO

-- | Encode a RTCM3Msg to a line of JSON.
encodeLine :: RTCM3Msg -> ByteString
encodeLine v = BL.toStrict $ toLazyByteString $ encodeToBuilder (toJSON v) <> "\n"

main :: IO ()
main =
  runResourceT $
    sourceHandle stdin  =$=
      conduitDecode     =$=
      CL.map encodeLine $$
      sinkHandle stdout
