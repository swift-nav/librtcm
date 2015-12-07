-- |
-- Module:      Data.RTCM3.TH
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Mark Fine <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- Templated generation of RTCM3 interfaces.

module Data.RTCM3.TH where

import BasicPrelude hiding ( length )
import Data.Binary
import Data.ByteString.Lazy
import Data.RTCM3.Types
import Language.Haskell.TH

-- | Derive ToRTCM3 typeclass, given an RTCM3 message number name and the
-- name of the implemented type.
deriveRTCM3 :: Name -> Q [Dec]
deriveRTCM3 name =
  [d|instance ToRTCM3 $(conT name) where
       toRTCM3 msg = Msg (fromIntegral $ length payload) (toStrict payload) where
         payload = encode msg
    |]
