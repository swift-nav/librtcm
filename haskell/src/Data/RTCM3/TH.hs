{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module:      Data.RTCM3.TH
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- Templated generation of RTCM3 interfaces.

module Data.RTCM3.TH where

import BasicPrelude         hiding (length)
import Data.Binary
import Data.ByteString
import Data.ByteString.Lazy hiding (length)
import Data.RTCM3.Types
import Language.Haskell.TH

-- | Derive ToRTCM3 typeclass, given an RTCM3 message number name and the
-- name of the implemented type.
deriveRTCM3 :: Name -> Q [Dec]
deriveRTCM3 name =
  [d|instance ToRTCM3 $(conT name) where
       toRTCM3 n = Msg len (Bytes pay) crc where
         pay = toStrict $ encode n
         len = fromIntegral $ length pay
         crc = checkCrc len pay
    |]
