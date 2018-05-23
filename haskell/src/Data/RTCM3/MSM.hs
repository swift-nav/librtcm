{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module:      Data.RTCM3.MSM
-- Copyright:   Copyright (C) 2018 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- RTCMv3 messages for Multiple Signal Messages.

module Data.RTCM3.MSM
  ( module Data.RTCM3.MSM
  ) where

import           BasicPrelude
import           Control.Lens
import           Data.Aeson.TH
import           Data.Binary
import           Data.Binary.Bits
import qualified Data.Binary.Bits.Get as B
import qualified Data.Binary.Bits.Put as B
import           Data.Bits
import           Data.RTCM3.TH

{-# ANN module ("HLint: ignore Use camelCase"::String) #-}

-- | MsmHeader.
--
-- MSM observation header.
data MsmHeader = MsmHeader
  { _msmHeader_num               :: Word16
    -- ^ Message number.
  , _msmHeader_station           :: Word16
    -- ^ Reference station id.
  , _msmHeader_epoch             :: Word32
    -- ^ GNSS epoch time.
  , _msmHeader_multiple          :: Bool
    -- ^ Multiple message indicator.
  , _msmHeader_iods              :: Word8
    -- ^ Issue of data station.
  , _msmHeader_reserved          :: Word8
    -- ^ Reserved.
  , _msmHeader_clockSteering     :: Word8
    -- ^ Clock steering indicator.
  , _msmHeader_externalClock     :: Word8
    -- ^ External clock indicator.
  , _msmHeader_smoothing         :: Bool
    -- ^ GNSS divergence-free smoothing indicator.
  , _msmHeader_smoothingInterval :: Word8
    -- ^ GNSS smoothing interval.
  , _msmHeader_satelliteMask     :: Word64
    -- ^ GNSS satellite mask.
  , _msmHeader_signalMask        :: Word32
    -- ^ GNSS signal mask.
  , _msmHeader_cellMask          :: Word64
    -- ^ GNSS cell mask.
  } deriving ( Show, Read, Eq )

$(makeLenses ''MsmHeader)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msmHeader_" . stripPrefix "_msmHeader_"} ''MsmHeader)

instance BinaryBit MsmHeader where
  getBits _n = do
    _msmHeader_num               <- B.getWord16be 12
    _msmHeader_station           <- B.getWord16be 12
    _msmHeader_epoch             <- B.getWord32be 30
    _msmHeader_multiple          <- B.getBool
    _msmHeader_iods              <- B.getWord8 3
    _msmHeader_reserved          <- B.getWord8 7
    _msmHeader_clockSteering     <- B.getWord8 2
    _msmHeader_externalClock     <- B.getWord8 2
    _msmHeader_smoothing         <- B.getBool
    _msmHeader_smoothingInterval <- B.getWord8 3
    _msmHeader_satelliteMask     <- B.getWord64be 64
    _msmHeader_signalMask        <- B.getWord32be 32
    let x = min 64 $ popCount _msmHeader_satelliteMask * popCount _msmHeader_signalMask
    _msmHeader_cellMask          <- B.getWord64be x
    pure MsmHeader {..}

  putBits _n MsmHeader {..} = do
    B.putWord16be 12 _msmHeader_num
    B.putWord16be 12 _msmHeader_station
    B.putWord32be 30 _msmHeader_epoch
    B.putBool        _msmHeader_multiple
    B.putWord8 3     _msmHeader_iods
    B.putWord8 7     _msmHeader_reserved
    B.putWord8 2     _msmHeader_clockSteering
    B.putWord8 2     _msmHeader_externalClock
    B.putBool        _msmHeader_smoothing
    B.putWord8 3     _msmHeader_smoothingInterval
    B.putWord64be 64 _msmHeader_satelliteMask
    B.putWord32be 32 _msmHeader_signalMask
    let x = min 64 $ popCount _msmHeader_satelliteMask * popCount _msmHeader_signalMask
    B.putWord64be x  _msmHeader_cellMask

msg1074 :: Word16
msg1074 = 1074

-- | Message 1074
--
-- RTCMv3 message 1074.
--
-- See RTCM spec and GLONASS signal specification for more information
-- about these fields.
data Msg1074 = Msg1074
  { _msg1074_header :: MsmHeader
    -- ^ MSM header.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1074)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1074_" . stripPrefix "_msg1074_"} ''Msg1074)

instance Binary Msg1074 where
  get = B.runBitGet $ do
    _msg1074_header <- getBits 0
    pure Msg1074 {..}

  put Msg1074 {..} = B.runBitPut $ do
    putBits 0 _msg1074_header

$(deriveRTCM3 ''Msg1074)
