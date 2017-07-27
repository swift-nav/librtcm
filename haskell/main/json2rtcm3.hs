{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Main
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- JSON to RTCM3 tool - reads RTCM3 JSON from stdin and sends RTCM3 binary
-- to stdout.

import BasicPrelude                      hiding (lines, mapMaybe)
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.List
import Data.Conduit.Serialization.Binary
import Data.RTCM3
import System.IO

-- | Decode a line of JSON to RTCM3.
decodeLine :: ByteString -> Maybe RTCM3Msg
decodeLine = decodeStrict

main :: IO ()
main =
  runResourceT $
    sourceHandle stdin    =$=
      lines               =$=
      mapMaybe decodeLine =$=
      conduitEncode       $$
      sinkHandle stdout
