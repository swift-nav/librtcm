{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module:      Data.RTCM3.SSR
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- RTCMv3 messages for State Space Representation parameters.

module Data.RTCM3.SSR
  ( module Data.RTCM3.SSR
  ) where

import           BasicPrelude
import           Control.Lens
import           Data.Aeson.TH
import           Data.Binary
import           Data.Binary.Bits
import qualified Data.Binary.Bits.Get as B
import qualified Data.Binary.Bits.Put as B
import           Data.Int
import           Data.RTCM3.Extras
import           Data.RTCM3.TH

{-# ANN module ("HLint: ignore Use camelCase"::String) #-}

-- | GpsOrbitCorrectionHeader.
--
-- GPS orbit correction header.
data GpsOrbitCorrectionHeader = GpsOrbitCorrectionHeader
  { _gpsOrbitCorrectionHeader_num            :: Word16
    -- ^ Message number.
  , _gpsOrbitCorrectionHeader_epochs         :: Word32
    -- ^ GPS epoch time.
  , _gpsOrbitCorrectionHeader_updateInterval :: Word8
    -- ^ SSR update interval.
  , _gpsOrbitCorrectionHeader_multiple       :: Bool
    -- ^ Multiple message indicator.
  , _gpsOrbitCorrectionHeader_datum          :: Bool
    -- ^ Satellite reference datum.
  , _gpsOrbitCorrectionHeader_iod            :: Word8
    -- ^ IOD SSR.
  , _gpsOrbitCorrectionHeader_provider       :: Word16
    -- ^ SSR provider id.
  , _gpsOrbitCorrectionHeader_solution       :: Word8
    -- ^ SSR solution id.
  , _gpsOrbitCorrectionHeader_n              :: Word8
    -- ^ Number of satellites.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsOrbitCorrectionHeader)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_gpsOrbitCorrectionHeader_" . stripPrefix "_gpsOrbitCorrectionHeader_"} ''GpsOrbitCorrectionHeader)

instance BinaryBit GpsOrbitCorrectionHeader where
  getBits _n = do
    _gpsOrbitCorrectionHeader_num            <- B.getWord16be 12
    _gpsOrbitCorrectionHeader_epochs         <- B.getWord32be 20
    _gpsOrbitCorrectionHeader_updateInterval <- B.getWord8 4
    _gpsOrbitCorrectionHeader_multiple       <- B.getBool
    _gpsOrbitCorrectionHeader_datum          <- B.getBool
    _gpsOrbitCorrectionHeader_iod            <- B.getWord8 4
    _gpsOrbitCorrectionHeader_provider       <- B.getWord16be 16
    _gpsOrbitCorrectionHeader_solution       <- B.getWord8 4
    _gpsOrbitCorrectionHeader_n              <- B.getWord8 6
    pure GpsOrbitCorrectionHeader {..}

  putBits _n GpsOrbitCorrectionHeader {..} = do
    B.putWord16be 12 _gpsOrbitCorrectionHeader_num
    B.putWord32be 20 _gpsOrbitCorrectionHeader_epochs
    B.putWord8 4     _gpsOrbitCorrectionHeader_updateInterval
    B.putBool        _gpsOrbitCorrectionHeader_multiple
    B.putBool        _gpsOrbitCorrectionHeader_datum
    B.putWord8 4     _gpsOrbitCorrectionHeader_iod
    B.putWord16be 16 _gpsOrbitCorrectionHeader_provider
    B.putWord8 4     _gpsOrbitCorrectionHeader_solution
    B.putWord8 6     _gpsOrbitCorrectionHeader_n

-- | GpsOrbitCorrectionMessage.
--
-- GPS orbit correction message.
data GpsOrbitCorrection = GpsOrbitCorrection
  { _gpsOrbitCorrection_sat                :: Word8
    -- ^ GPS satellite id.
  , _gpsOrbitCorrection_iode               :: Word8
    -- ^ GPS IODE.
  , _gpsOrbitCorrection_deltaRadial        :: Int32
    -- ^ Delta Radial.
  , _gpsOrbitCorrection_deltaAlongTrack    :: Int32
    -- ^ Delta Along-Track.
  , _gpsOrbitCorrection_deltaCrossTrack    :: Int32
    -- ^ Delta Cross-Track.
  , _gpsOrbitCorrection_dotDeltaRadial     :: Int32
    -- ^ Dot Delta Radial.
  , _gpsOrbitCorrection_dotDeltaAlongTrack :: Int32
    -- ^ Dot Delta Along-Track.
  , _gpsOrbitCorrection_dotDeltaCrossTrack :: Int32
    -- ^ Dot Delta Cross-Track.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsOrbitCorrection)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_gpsOrbitCorrection_" . stripPrefix "_gpsOrbitCorrection_"} ''GpsOrbitCorrection)

instance BinaryBit GpsOrbitCorrection where
  getBits _n = do
    _gpsOrbitCorrection_sat                <- B.getWord8 6
    _gpsOrbitCorrection_iode               <- B.getWord8 8
    _gpsOrbitCorrection_deltaRadial        <- getInt32be 22
    _gpsOrbitCorrection_deltaAlongTrack    <- getInt32be 20
    _gpsOrbitCorrection_deltaCrossTrack    <- getInt32be 20
    _gpsOrbitCorrection_dotDeltaRadial     <- getInt32be 21
    _gpsOrbitCorrection_dotDeltaAlongTrack <- getInt32be 19
    _gpsOrbitCorrection_dotDeltaCrossTrack <- getInt32be 19
    pure GpsOrbitCorrection {..}

  putBits _n GpsOrbitCorrection {..} = do
    B.putWord8 6  _gpsOrbitCorrection_sat
    B.putWord8 8  _gpsOrbitCorrection_iode
    putInt32be 22 _gpsOrbitCorrection_deltaRadial
    putInt32be 20 _gpsOrbitCorrection_deltaAlongTrack
    putInt32be 20 _gpsOrbitCorrection_deltaCrossTrack
    putInt32be 21 _gpsOrbitCorrection_dotDeltaRadial
    putInt32be 19 _gpsOrbitCorrection_dotDeltaAlongTrack
    putInt32be 19 _gpsOrbitCorrection_dotDeltaCrossTrack

-- | GpsClockCorrectionHeader.
--
-- GPS clock correction message.
data GpsClockCorrectionHeader = GpsClockCorrectionHeader
  { _gpsClockCorrectionHeader_num            :: Word16
    -- ^ Message number.
  , _gpsClockCorrectionHeader_epochs         :: Word32
    -- ^ GPS epoch time.
  , _gpsClockCorrectionHeader_updateInterval :: Word8
    -- ^ SSR update interval.
  , _gpsClockCorrectionHeader_multiple       :: Bool
    -- ^ Multiple message indicator.
  , _gpsClockCorrectionHeader_iod            :: Word8
    -- ^ IOD SSR.
  , _gpsClockCorrectionHeader_provider       :: Word16
    -- ^ SSR provider id.
  , _gpsClockCorrectionHeader_solution       :: Word8
    -- ^ SSR solution id.
  , _gpsClockCorrectionHeader_n              :: Word8
    -- ^ Number of satellites.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsClockCorrectionHeader)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_gpsClockCorrectionHeader_" . stripPrefix "_gpsClockCorrectionHeader_"} ''GpsClockCorrectionHeader)

instance BinaryBit GpsClockCorrectionHeader where
  getBits _n = do
    _gpsClockCorrectionHeader_num            <- B.getWord16be 12
    _gpsClockCorrectionHeader_epochs         <- B.getWord32be 20
    _gpsClockCorrectionHeader_updateInterval <- B.getWord8 4
    _gpsClockCorrectionHeader_multiple       <- B.getBool
    _gpsClockCorrectionHeader_iod            <- B.getWord8 4
    _gpsClockCorrectionHeader_provider       <- B.getWord16be 16
    _gpsClockCorrectionHeader_solution       <- B.getWord8 4
    _gpsClockCorrectionHeader_n              <- B.getWord8 6
    pure GpsClockCorrectionHeader {..}

  putBits _n GpsClockCorrectionHeader {..} = do
    B.putWord16be 12 _gpsClockCorrectionHeader_num
    B.putWord32be 20 _gpsClockCorrectionHeader_epochs
    B.putWord8 4     _gpsClockCorrectionHeader_updateInterval
    B.putBool        _gpsClockCorrectionHeader_multiple
    B.putWord8 4     _gpsClockCorrectionHeader_iod
    B.putWord16be 16 _gpsClockCorrectionHeader_provider
    B.putWord8 4     _gpsClockCorrectionHeader_solution
    B.putWord8 6     _gpsClockCorrectionHeader_n

-- | GpsClockCorrectionMessage.
--
-- GPS clock correction message.
data GpsClockCorrection = GpsClockCorrection
  { _gpsClockCorrection_sat          :: Word8
    -- ^ GPS satellite id.
  , _gpsClockCorrection_deltaClockC0 :: Int32
    -- ^ Delta clock C0.
  , _gpsClockCorrection_deltaClockC1 :: Int32
    -- ^ Delta clock C1.
  , _gpsClockCorrection_deltaClockC2 :: Int32
    -- ^ Delta clock C2.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsClockCorrection)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_gpsClockCorrection_" . stripPrefix "_gpsClockCorrection_"} ''GpsClockCorrection)

instance BinaryBit GpsClockCorrection where
  getBits _n = do
    _gpsClockCorrection_sat          <- B.getWord8 6
    _gpsClockCorrection_deltaClockC0 <- getInt32be 22
    _gpsClockCorrection_deltaClockC1 <- getInt32be 21
    _gpsClockCorrection_deltaClockC2 <- getInt32be 27
    pure GpsClockCorrection {..}

  putBits _n GpsClockCorrection {..} = do
    B.putWord8 6  _gpsClockCorrection_sat
    putInt32be 22 _gpsClockCorrection_deltaClockC0
    putInt32be 21 _gpsClockCorrection_deltaClockC1
    putInt32be 27 _gpsClockCorrection_deltaClockC2

-- | GlonassOrbitCorrectionHeader.
--
-- GLONASS orbit correction header.
data GlonassOrbitCorrectionHeader = GlonassOrbitCorrectionHeader
  { _glonassOrbitCorrectionHeader_num            :: Word16
    -- ^ Message number.
  , _glonassOrbitCorrectionHeader_epochs         :: Word32
    -- ^ GLONASS epoch time.
  , _glonassOrbitCorrectionHeader_updateInterval :: Word8
    -- ^ SSR update interval.
  , _glonassOrbitCorrectionHeader_multiple       :: Bool
    -- ^ Multiple message indicator.
  , _glonassOrbitCorrectionHeader_datum          :: Bool
    -- ^ Satellite reference datum.
  , _glonassOrbitCorrectionHeader_iod            :: Word8
    -- ^ IOD SSR.
  , _glonassOrbitCorrectionHeader_provider       :: Word16
    -- ^ SSR provider id.
  , _glonassOrbitCorrectionHeader_solution       :: Word8
    -- ^ SSR solution id.
  , _glonassOrbitCorrectionHeader_n              :: Word8
    -- ^ Number of satellites.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GlonassOrbitCorrectionHeader)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_glonassOrbitCorrectionHeader_" . stripPrefix "_glonassOrbitCorrectionHeader_"} ''GlonassOrbitCorrectionHeader)

instance BinaryBit GlonassOrbitCorrectionHeader where
  getBits _n = do
    _glonassOrbitCorrectionHeader_num            <- B.getWord16be 12
    _glonassOrbitCorrectionHeader_epochs         <- B.getWord32be 17
    _glonassOrbitCorrectionHeader_updateInterval <- B.getWord8 4
    _glonassOrbitCorrectionHeader_multiple       <- B.getBool
    _glonassOrbitCorrectionHeader_datum          <- B.getBool
    _glonassOrbitCorrectionHeader_iod            <- B.getWord8 4
    _glonassOrbitCorrectionHeader_provider       <- B.getWord16be 16
    _glonassOrbitCorrectionHeader_solution       <- B.getWord8 4
    _glonassOrbitCorrectionHeader_n              <- B.getWord8 6
    pure GlonassOrbitCorrectionHeader {..}

  putBits _n GlonassOrbitCorrectionHeader {..} = do
    B.putWord16be 12 _glonassOrbitCorrectionHeader_num
    B.putWord32be 17 _glonassOrbitCorrectionHeader_epochs
    B.putWord8 4     _glonassOrbitCorrectionHeader_updateInterval
    B.putBool        _glonassOrbitCorrectionHeader_multiple
    B.putBool        _glonassOrbitCorrectionHeader_datum
    B.putWord8 4     _glonassOrbitCorrectionHeader_iod
    B.putWord16be 16 _glonassOrbitCorrectionHeader_provider
    B.putWord8 4     _glonassOrbitCorrectionHeader_solution
    B.putWord8 6     _glonassOrbitCorrectionHeader_n

-- | GlonassOrbitCorrectionMessage.
--
-- GLONASS orbit correction message.
data GlonassOrbitCorrection = GlonassOrbitCorrection
  { _glonassOrbitCorrection_sat                :: Word8
    -- ^ GLONASS satellite id.
  , _glonassOrbitCorrection_iode               :: Word8
    -- ^ GLONASS IODE.
  , _glonassOrbitCorrection_deltaRadial        :: Int32
    -- ^ Delta Radial.
  , _glonassOrbitCorrection_deltaAlongTrack    :: Int32
    -- ^ Delta Along-Track.
  , _glonassOrbitCorrection_deltaCrossTrack    :: Int32
    -- ^ Delta Cross-Track.
  , _glonassOrbitCorrection_dotDeltaRadial     :: Int32
    -- ^ Dot Delta Radial.
  , _glonassOrbitCorrection_dotDeltaAlongTrack :: Int32
    -- ^ Dot Delta Along-Track.
  , _glonassOrbitCorrection_dotDeltaCrossTrack :: Int32
    -- ^ Dot Delta Cross-Track.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GlonassOrbitCorrection)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_glonassOrbitCorrection_" . stripPrefix "_glonassOrbitCorrection_"} ''GlonassOrbitCorrection)

instance BinaryBit GlonassOrbitCorrection where
  getBits _n = do
    _glonassOrbitCorrection_sat                <- B.getWord8 5
    _glonassOrbitCorrection_iode               <- B.getWord8 8
    _glonassOrbitCorrection_deltaRadial        <- getInt32be 22
    _glonassOrbitCorrection_deltaAlongTrack    <- getInt32be 20
    _glonassOrbitCorrection_deltaCrossTrack    <- getInt32be 20
    _glonassOrbitCorrection_dotDeltaRadial     <- getInt32be 21
    _glonassOrbitCorrection_dotDeltaAlongTrack <- getInt32be 19
    _glonassOrbitCorrection_dotDeltaCrossTrack <- getInt32be 19
    pure GlonassOrbitCorrection {..}

  putBits _n GlonassOrbitCorrection {..} = do
    B.putWord8 5  _glonassOrbitCorrection_sat
    B.putWord8 8  _glonassOrbitCorrection_iode
    putInt32be 22 _glonassOrbitCorrection_deltaRadial
    putInt32be 20 _glonassOrbitCorrection_deltaAlongTrack
    putInt32be 20 _glonassOrbitCorrection_deltaCrossTrack
    putInt32be 21 _glonassOrbitCorrection_dotDeltaRadial
    putInt32be 19 _glonassOrbitCorrection_dotDeltaAlongTrack
    putInt32be 19 _glonassOrbitCorrection_dotDeltaCrossTrack

-- | GlonassClockCorrectionHeader.
--
-- GLONASS clock correction message.
data GlonassClockCorrectionHeader = GlonassClockCorrectionHeader
  { _glonassClockCorrectionHeader_num            :: Word16
    -- ^ Message number.
  , _glonassClockCorrectionHeader_epochs         :: Word32
    -- ^ GLONASS epoch time.
  , _glonassClockCorrectionHeader_updateInterval :: Word8
    -- ^ SSR update interval.
  , _glonassClockCorrectionHeader_multiple       :: Bool
    -- ^ Multiple message indicator.
  , _glonassClockCorrectionHeader_iod            :: Word8
    -- ^ IOD SSR.
  , _glonassClockCorrectionHeader_provider       :: Word16
    -- ^ SSR provider id.
  , _glonassClockCorrectionHeader_solution       :: Word8
    -- ^ SSR solution id.
  , _glonassClockCorrectionHeader_n              :: Word8
    -- ^ Number of satellites.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GlonassClockCorrectionHeader)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_glonassClockCorrectionHeader_" . stripPrefix "_glonassClockCorrectionHeader_"} ''GlonassClockCorrectionHeader)

instance BinaryBit GlonassClockCorrectionHeader where
  getBits _n = do
    _glonassClockCorrectionHeader_num            <- B.getWord16be 12
    _glonassClockCorrectionHeader_epochs         <- B.getWord32be 17
    _glonassClockCorrectionHeader_updateInterval <- B.getWord8 4
    _glonassClockCorrectionHeader_multiple       <- B.getBool
    _glonassClockCorrectionHeader_iod            <- B.getWord8 4
    _glonassClockCorrectionHeader_provider       <- B.getWord16be 16
    _glonassClockCorrectionHeader_solution       <- B.getWord8 4
    _glonassClockCorrectionHeader_n              <- B.getWord8 6
    pure GlonassClockCorrectionHeader {..}

  putBits _n GlonassClockCorrectionHeader {..} = do
    B.putWord16be 12 _glonassClockCorrectionHeader_num
    B.putWord32be 17 _glonassClockCorrectionHeader_epochs
    B.putWord8 4     _glonassClockCorrectionHeader_updateInterval
    B.putBool        _glonassClockCorrectionHeader_multiple
    B.putWord8 4     _glonassClockCorrectionHeader_iod
    B.putWord16be 16 _glonassClockCorrectionHeader_provider
    B.putWord8 4     _glonassClockCorrectionHeader_solution
    B.putWord8 6     _glonassClockCorrectionHeader_n

-- | GlonassClockCorrectionMessage.
--
-- GLONASS clock correction message.
data GlonassClockCorrection = GlonassClockCorrection
  { _glonassClockCorrection_sat          :: Word8
    -- ^ GLONASS satellite id.
  , _glonassClockCorrection_deltaClockC0 :: Int32
    -- ^ Delta clock C0.
  , _glonassClockCorrection_deltaClockC1 :: Int32
    -- ^ Delta clock C1.
  , _glonassClockCorrection_deltaClockC2 :: Int32
    -- ^ Delta clock C2.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GlonassClockCorrection)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_glonassClockCorrection_" . stripPrefix "_glonassClockCorrection_"} ''GlonassClockCorrection)

instance BinaryBit GlonassClockCorrection where
  getBits _n = do
    _glonassClockCorrection_sat          <- B.getWord8 5
    _glonassClockCorrection_deltaClockC0 <- getInt32be 22
    _glonassClockCorrection_deltaClockC1 <- getInt32be 21
    _glonassClockCorrection_deltaClockC2 <- getInt32be 27
    pure GlonassClockCorrection {..}

  putBits _n GlonassClockCorrection {..} = do
    B.putWord8 5  _glonassClockCorrection_sat
    putInt32be 22 _glonassClockCorrection_deltaClockC0
    putInt32be 21 _glonassClockCorrection_deltaClockC1
    putInt32be 27 _glonassClockCorrection_deltaClockC2

-- | GpsOrbitClockCorrectionHeader.
--
-- GPS orbit and clock correction header.

data GpsOrbitClockCorrectionHeader = GpsOrbitClockCorrectionHeader
  { _gpsOrbitClockCorrectionHeader_num            :: Word16
    -- ^ Message number.
  , _gpsOrbitClockCorrectionHeader_epochs         :: Word32
    -- ^ GPS epoch time.
  , _gpsOrbitClockCorrectionHeader_updateInterval :: Word8
    -- ^ SSR update interval.
  , _gpsOrbitClockCorrectionHeader_multiple       :: Bool
    -- ^ Multiple message indicator.
  , _gpsOrbitClockCorrectionHeader_datum          :: Bool
    -- ^ Satellite reference datum.
  , _gpsOrbitClockCorrectionHeader_iod            :: Word8
    -- ^ IOD SSR.
  , _gpsOrbitClockCorrectionHeader_provider       :: Word16
    -- ^ SSR provider id.
  , _gpsOrbitClockCorrectionHeader_solution       :: Word8
    -- ^ SSR solution id.
  , _gpsOrbitClockCorrectionHeader_n              :: Word8
    -- ^ Number of satellites.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsOrbitClockCorrectionHeader)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_gpsOrbitClockCorrectionHeader_" . stripPrefix "_gpsOrbitClockCorrectionHeader_"} ''GpsOrbitClockCorrectionHeader)

instance BinaryBit GpsOrbitClockCorrectionHeader where
  getBits _n = do
    _gpsOrbitClockCorrectionHeader_num            <- B.getWord16be 12
    _gpsOrbitClockCorrectionHeader_epochs         <- B.getWord32be 20
    _gpsOrbitClockCorrectionHeader_updateInterval <- B.getWord8 4
    _gpsOrbitClockCorrectionHeader_multiple       <- B.getBool
    _gpsOrbitClockCorrectionHeader_datum          <- B.getBool
    _gpsOrbitClockCorrectionHeader_iod            <- B.getWord8 4
    _gpsOrbitClockCorrectionHeader_provider       <- B.getWord16be 16
    _gpsOrbitClockCorrectionHeader_solution       <- B.getWord8 4
    _gpsOrbitClockCorrectionHeader_n              <- B.getWord8 6
    pure GpsOrbitClockCorrectionHeader {..}

  putBits _n GpsOrbitClockCorrectionHeader {..} = do
    B.putWord16be 12 _gpsOrbitClockCorrectionHeader_num
    B.putWord32be 20 _gpsOrbitClockCorrectionHeader_epochs
    B.putWord8 4     _gpsOrbitClockCorrectionHeader_updateInterval
    B.putBool        _gpsOrbitClockCorrectionHeader_multiple
    B.putBool        _gpsOrbitClockCorrectionHeader_datum
    B.putWord8 4     _gpsOrbitClockCorrectionHeader_iod
    B.putWord16be 16 _gpsOrbitClockCorrectionHeader_provider
    B.putWord8 4     _gpsOrbitClockCorrectionHeader_solution
    B.putWord8 6     _gpsOrbitClockCorrectionHeader_n

-- | GpsOrbitClockCorrectionMessage.
--
-- GPS orbit correction message.
data GpsOrbitClockCorrection = GpsOrbitClockCorrection
  { _gpsOrbitClockCorrection_sat                :: Word8
    -- ^ GPS satellite id.
  , _gpsOrbitClockCorrection_iode               :: Word8
    -- ^ GPS IODE.
  , _gpsOrbitClockCorrection_deltaRadial        :: Int32
    -- ^ Delta Radial.
  , _gpsOrbitClockCorrection_deltaAlongTrack    :: Int32
    -- ^ Delta Along-Track.
  , _gpsOrbitClockCorrection_deltaCrossTrack    :: Int32
    -- ^ Delta Cross-Track.
  , _gpsOrbitClockCorrection_dotDeltaRadial     :: Int32
    -- ^ Dot Delta Radial.
  , _gpsOrbitClockCorrection_dotDeltaAlongTrack :: Int32
    -- ^ Dot Delta Along-Track.
  , _gpsOrbitClockCorrection_dotDeltaCrossTrack :: Int32
    -- ^ Dot Delta Cross-Track.
  , _gpsOrbitClockCorrection_deltaClockC0       :: Int32
    -- ^ Delta clock C0.
  , _gpsOrbitClockCorrection_deltaClockC1       :: Int32
    -- ^ Delta clock C1.
  , _gpsOrbitClockCorrection_deltaClockC2       :: Int32
    -- ^ Delta clock C2.

  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsOrbitClockCorrection)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_gpsOrbitClockCorrection_" . stripPrefix "_gpsOrbitClockCorrection_"} ''GpsOrbitClockCorrection)

instance BinaryBit GpsOrbitClockCorrection where
  getBits _n = do
    _gpsOrbitClockCorrection_sat                <- B.getWord8 6
    _gpsOrbitClockCorrection_iode               <- B.getWord8 8
    _gpsOrbitClockCorrection_deltaRadial        <- getInt32be 22
    _gpsOrbitClockCorrection_deltaAlongTrack    <- getInt32be 20
    _gpsOrbitClockCorrection_deltaCrossTrack    <- getInt32be 20
    _gpsOrbitClockCorrection_dotDeltaRadial     <- getInt32be 21
    _gpsOrbitClockCorrection_dotDeltaAlongTrack <- getInt32be 19
    _gpsOrbitClockCorrection_dotDeltaCrossTrack <- getInt32be 19
    _gpsOrbitClockCorrection_deltaClockC0       <- getInt32be 22
    _gpsOrbitClockCorrection_deltaClockC1       <- getInt32be 21
    _gpsOrbitClockCorrection_deltaClockC2       <- getInt32be 27
    pure GpsOrbitClockCorrection {..}

  putBits _n GpsOrbitClockCorrection {..} = do
    B.putWord8 6  _gpsOrbitClockCorrection_sat
    B.putWord8 8  _gpsOrbitClockCorrection_iode
    putInt32be 22 _gpsOrbitClockCorrection_deltaRadial
    putInt32be 20 _gpsOrbitClockCorrection_deltaAlongTrack
    putInt32be 20 _gpsOrbitClockCorrection_deltaCrossTrack
    putInt32be 21 _gpsOrbitClockCorrection_dotDeltaRadial
    putInt32be 19 _gpsOrbitClockCorrection_dotDeltaAlongTrack
    putInt32be 19 _gpsOrbitClockCorrection_dotDeltaCrossTrack
    putInt32be 22 _gpsOrbitClockCorrection_deltaClockC0
    putInt32be 21 _gpsOrbitClockCorrection_deltaClockC1
    putInt32be 27 _gpsOrbitClockCorrection_deltaClockC2

-- | GlonassOrbitClockCorrectionHeader.
--
-- GLONASS orbit correction header.
data GlonassOrbitClockCorrectionHeader = GlonassOrbitClockCorrectionHeader
  { _glonassOrbitClockCorrectionHeader_num            :: Word16
    -- ^ Message number.
  , _glonassOrbitClockCorrectionHeader_epochs         :: Word32
    -- ^ GLONASS epoch time.
  , _glonassOrbitClockCorrectionHeader_updateInterval :: Word8
    -- ^ SSR update interval.
  , _glonassOrbitClockCorrectionHeader_multiple       :: Bool
    -- ^ Multiple message indicator.
  , _glonassOrbitClockCorrectionHeader_datum          :: Bool
    -- ^ Satellite reference datum.
  , _glonassOrbitClockCorrectionHeader_iod            :: Word8
    -- ^ IOD SSR.
  , _glonassOrbitClockCorrectionHeader_provider       :: Word16
    -- ^ SSR provider id.
  , _glonassOrbitClockCorrectionHeader_solution       :: Word8
    -- ^ SSR solution id.
  , _glonassOrbitClockCorrectionHeader_n              :: Word8
    -- ^ Number of satellites.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GlonassOrbitClockCorrectionHeader)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_glonassOrbitClockCorrectionHeader_" . stripPrefix "_glonassOrbitClockCorrectionHeader_"} ''GlonassOrbitClockCorrectionHeader)

instance BinaryBit GlonassOrbitClockCorrectionHeader where
  getBits _n = do
    _glonassOrbitClockCorrectionHeader_num            <- B.getWord16be 12
    _glonassOrbitClockCorrectionHeader_epochs         <- B.getWord32be 17
    _glonassOrbitClockCorrectionHeader_updateInterval <- B.getWord8 4
    _glonassOrbitClockCorrectionHeader_multiple       <- B.getBool
    _glonassOrbitClockCorrectionHeader_datum          <- B.getBool
    _glonassOrbitClockCorrectionHeader_iod            <- B.getWord8 4
    _glonassOrbitClockCorrectionHeader_provider       <- B.getWord16be 16
    _glonassOrbitClockCorrectionHeader_solution       <- B.getWord8 4
    _glonassOrbitClockCorrectionHeader_n              <- B.getWord8 6
    pure GlonassOrbitClockCorrectionHeader {..}

  putBits _n GlonassOrbitClockCorrectionHeader {..} = do
    B.putWord16be 12 _glonassOrbitClockCorrectionHeader_num
    B.putWord32be 17 _glonassOrbitClockCorrectionHeader_epochs
    B.putWord8 4     _glonassOrbitClockCorrectionHeader_updateInterval
    B.putBool        _glonassOrbitClockCorrectionHeader_multiple
    B.putBool        _glonassOrbitClockCorrectionHeader_datum
    B.putWord8 4     _glonassOrbitClockCorrectionHeader_iod
    B.putWord16be 16 _glonassOrbitClockCorrectionHeader_provider
    B.putWord8 4     _glonassOrbitClockCorrectionHeader_solution
    B.putWord8 6     _glonassOrbitClockCorrectionHeader_n

-- | GlonassOrbitClockCorrectionMessage.
--
-- GLONASS orbit correction message.
data GlonassOrbitClockCorrection = GlonassOrbitClockCorrection
  { _glonassOrbitClockCorrection_sat                :: Word8
    -- ^ GLONASS satellite id.
  , _glonassOrbitClockCorrection_iode               :: Word8
    -- ^ GLONASS IODE.
  , _glonassOrbitClockCorrection_deltaRadial        :: Int32
    -- ^ Delta Radial.
  , _glonassOrbitClockCorrection_deltaAlongTrack    :: Int32
    -- ^ Delta Along-Track.
  , _glonassOrbitClockCorrection_deltaCrossTrack    :: Int32
    -- ^ Delta Cross-Track.
  , _glonassOrbitClockCorrection_dotDeltaRadial     :: Int32
    -- ^ Dot Delta Radial.
  , _glonassOrbitClockCorrection_dotDeltaAlongTrack :: Int32
    -- ^ Dot Delta Along-Track.
  , _glonassOrbitClockCorrection_dotDeltaCrossTrack :: Int32
    -- ^ Dot Delta Cross-Track.
  , _glonassOrbitClockCorrection_deltaClockC0       :: Int32
    -- ^ Delta clock C0.
  , _glonassOrbitClockCorrection_deltaClockC1       :: Int32
    -- ^ Delta clock C1.
  , _glonassOrbitClockCorrection_deltaClockC2       :: Int32
    -- ^ Delta clock C2.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GlonassOrbitClockCorrection)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_glonassOrbitClockCorrection_" . stripPrefix "_glonassOrbitClockCorrection_"} ''GlonassOrbitClockCorrection)

instance BinaryBit GlonassOrbitClockCorrection where
  getBits _n = do
    _glonassOrbitClockCorrection_sat                <- B.getWord8 5
    _glonassOrbitClockCorrection_iode               <- B.getWord8 8
    _glonassOrbitClockCorrection_deltaRadial        <- getInt32be 22
    _glonassOrbitClockCorrection_deltaAlongTrack    <- getInt32be 20
    _glonassOrbitClockCorrection_deltaCrossTrack    <- getInt32be 20
    _glonassOrbitClockCorrection_dotDeltaRadial     <- getInt32be 21
    _glonassOrbitClockCorrection_dotDeltaAlongTrack <- getInt32be 19
    _glonassOrbitClockCorrection_dotDeltaCrossTrack <- getInt32be 19
    _glonassOrbitClockCorrection_deltaClockC0       <- getInt32be 22
    _glonassOrbitClockCorrection_deltaClockC1       <- getInt32be 21
    _glonassOrbitClockCorrection_deltaClockC2       <- getInt32be 27
    pure GlonassOrbitClockCorrection {..}

  putBits _n GlonassOrbitClockCorrection {..} = do
    B.putWord8 5  _glonassOrbitClockCorrection_sat
    B.putWord8 8  _glonassOrbitClockCorrection_iode
    putInt32be 22 _glonassOrbitClockCorrection_deltaRadial
    putInt32be 20 _glonassOrbitClockCorrection_deltaAlongTrack
    putInt32be 20 _glonassOrbitClockCorrection_deltaCrossTrack
    putInt32be 21 _glonassOrbitClockCorrection_dotDeltaRadial
    putInt32be 19 _glonassOrbitClockCorrection_dotDeltaAlongTrack
    putInt32be 19 _glonassOrbitClockCorrection_dotDeltaCrossTrack
    putInt32be 22 _glonassOrbitClockCorrection_deltaClockC0
    putInt32be 21 _glonassOrbitClockCorrection_deltaClockC1
    putInt32be 27 _glonassOrbitClockCorrection_deltaClockC2

-- | GpsCodeBiasCorrectionHeader.
--
-- GPS code bias correction header.
data GpsCodeBiasCorrectionHeader = GpsCodeBiasCorrectionHeader
  { _gpsCodeBiasCorrectionHeader_num            :: Word16
    -- ^ Message number.
  , _gpsCodeBiasCorrectionHeader_epochs         :: Word32
    -- ^ GPS epoch time.
  , _gpsCodeBiasCorrectionHeader_updateInterval :: Word8
    -- ^ SSR update interval.
  , _gpsCodeBiasCorrectionHeader_multiple       :: Bool
    -- ^ Multiple message indicator.
  , _gpsCodeBiasCorrectionHeader_iod            :: Word8
    -- ^ IOD SSR.
  , _gpsCodeBiasCorrectionHeader_provider       :: Word16
    -- ^ SSR provider id.
  , _gpsCodeBiasCorrectionHeader_solution       :: Word8
    -- ^ SSR solution id.
  , _gpsCodeBiasCorrectionHeader_n              :: Word8
    -- ^ Number of satellites.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsCodeBiasCorrectionHeader)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_gpsCodeBiasCorrectionHeader_" . stripPrefix "_gpsCodeBiasCorrectionHeader_"} ''GpsCodeBiasCorrectionHeader)

instance BinaryBit GpsCodeBiasCorrectionHeader where
  getBits _n = do
    _gpsCodeBiasCorrectionHeader_num            <- B.getWord16be 12
    _gpsCodeBiasCorrectionHeader_epochs         <- B.getWord32be 20
    _gpsCodeBiasCorrectionHeader_updateInterval <- B.getWord8 4
    _gpsCodeBiasCorrectionHeader_multiple       <- B.getBool
    _gpsCodeBiasCorrectionHeader_iod            <- B.getWord8 4
    _gpsCodeBiasCorrectionHeader_provider       <- B.getWord16be 16
    _gpsCodeBiasCorrectionHeader_solution       <- B.getWord8 4
    _gpsCodeBiasCorrectionHeader_n              <- B.getWord8 6
    pure GpsCodeBiasCorrectionHeader {..}

  putBits _n GpsCodeBiasCorrectionHeader {..} = do
    B.putWord16be 12 _gpsCodeBiasCorrectionHeader_num
    B.putWord32be 20 _gpsCodeBiasCorrectionHeader_epochs
    B.putWord8 4     _gpsCodeBiasCorrectionHeader_updateInterval
    B.putBool        _gpsCodeBiasCorrectionHeader_multiple
    B.putWord8 4     _gpsCodeBiasCorrectionHeader_iod
    B.putWord16be 16 _gpsCodeBiasCorrectionHeader_provider
    B.putWord8 4     _gpsCodeBiasCorrectionHeader_solution
    B.putWord8 6     _gpsCodeBiasCorrectionHeader_n

-- | GpsCodeBias.
--
-- GPS code bias.
data GpsCodeBias = GpsCodeBias
  { _gpsCodeBias_signal   :: Word8
    -- ^ GPS signal.
  , _gpsCodeBias_codeBias :: Int16
    -- ^ GPS code bias.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsCodeBias)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_gpsCodeBias_" . stripPrefix "_gpsCodeBias_"} ''GpsCodeBias)

instance BinaryBit GpsCodeBias where
  getBits _n = do
    _gpsCodeBias_signal   <- B.getWord8 5
    _gpsCodeBias_codeBias <- getInt16be 14
    pure GpsCodeBias {..}

  putBits _n GpsCodeBias {..} = do
    B.putWord8 5  _gpsCodeBias_signal
    putInt16be 14 _gpsCodeBias_codeBias

-- | GpsCodeBiasCorrectionMessage.
--
-- GPS code bias correction message.
data GpsCodeBiasCorrection = GpsCodeBiasCorrection
  { _gpsCodeBiasCorrection_sat        :: Word8
    -- ^ GPS satellite id.
  , _gpsCodeBiasCorrection_n          :: Word8
    -- ^ Number of biases.
  , _gpsCodeBiasCorrection_codeBiases :: [GpsCodeBias]
    -- ^ GPS code biases.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsCodeBiasCorrection)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_gpsCodeBiasCorrection_" . stripPrefix "_gpsCodeBiasCorrection_"} ''GpsCodeBiasCorrection)

instance BinaryBit GpsCodeBiasCorrection where
  getBits _n = do
    _gpsCodeBiasCorrection_sat        <- B.getWord8 6
    _gpsCodeBiasCorrection_n          <- B.getWord8 5
    _gpsCodeBiasCorrection_codeBiases <- replicateM (fromIntegral _gpsCodeBiasCorrection_n) $ getBits 0
    pure GpsCodeBiasCorrection {..}

  putBits _n GpsCodeBiasCorrection {..} = do
    B.putWord8 6 _gpsCodeBiasCorrection_sat
    B.putWord8 5 _gpsCodeBiasCorrection_n
    forM_ _gpsCodeBiasCorrection_codeBiases $ putBits 0

-- | GlonassCodeBiasCorrectionHeader.
--
-- GLONASS code bias correction header.
data GlonassCodeBiasCorrectionHeader = GlonassCodeBiasCorrectionHeader
  { _glonassCodeBiasCorrectionHeader_num            :: Word16
    -- ^ Message number.
  , _glonassCodeBiasCorrectionHeader_epochs         :: Word32
    -- ^ GLONASS epoch time.
  , _glonassCodeBiasCorrectionHeader_updateInterval :: Word8
    -- ^ SSR update interval.
  , _glonassCodeBiasCorrectionHeader_multiple       :: Bool
    -- ^ Multiple message indicator.
  , _glonassCodeBiasCorrectionHeader_iod            :: Word8
    -- ^ IOD SSR.
  , _glonassCodeBiasCorrectionHeader_provider       :: Word16
    -- ^ SSR provider id.
  , _glonassCodeBiasCorrectionHeader_solution       :: Word8
    -- ^ SSR solution id.
  , _glonassCodeBiasCorrectionHeader_n              :: Word8
    -- ^ Number of satellites.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GlonassCodeBiasCorrectionHeader)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_glonassCodeBiasCorrectionHeader_" . stripPrefix "_glonassCodeBiasCorrectionHeader_"} ''GlonassCodeBiasCorrectionHeader)

instance BinaryBit GlonassCodeBiasCorrectionHeader where
  getBits _n = do
    _glonassCodeBiasCorrectionHeader_num            <- B.getWord16be 12
    _glonassCodeBiasCorrectionHeader_epochs         <- B.getWord32be 17
    _glonassCodeBiasCorrectionHeader_updateInterval <- B.getWord8 4
    _glonassCodeBiasCorrectionHeader_multiple       <- B.getBool
    _glonassCodeBiasCorrectionHeader_iod            <- B.getWord8 4
    _glonassCodeBiasCorrectionHeader_provider       <- B.getWord16be 16
    _glonassCodeBiasCorrectionHeader_solution       <- B.getWord8 4
    _glonassCodeBiasCorrectionHeader_n              <- B.getWord8 6
    pure GlonassCodeBiasCorrectionHeader {..}

  putBits _n GlonassCodeBiasCorrectionHeader {..} = do
    B.putWord16be 12 _glonassCodeBiasCorrectionHeader_num
    B.putWord32be 17 _glonassCodeBiasCorrectionHeader_epochs
    B.putWord8 4     _glonassCodeBiasCorrectionHeader_updateInterval
    B.putBool        _glonassCodeBiasCorrectionHeader_multiple
    B.putWord8 4     _glonassCodeBiasCorrectionHeader_iod
    B.putWord16be 16 _glonassCodeBiasCorrectionHeader_provider
    B.putWord8 4     _glonassCodeBiasCorrectionHeader_solution
    B.putWord8 6     _glonassCodeBiasCorrectionHeader_n

-- | GlonassCodeBias.
--
-- GLONASS code bias.
data GlonassCodeBias = GlonassCodeBias
  { _glonassCodeBias_signal   :: Word8
    -- ^ GLONASS signal.
  , _glonassCodeBias_codeBias :: Int16
    -- ^ GLONASS code bias.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GlonassCodeBias)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_glonassCodeBias_" . stripPrefix "_glonassCodeBias_"} ''GlonassCodeBias)

instance BinaryBit GlonassCodeBias where
  getBits _n = do
    _glonassCodeBias_signal   <- B.getWord8 5
    _glonassCodeBias_codeBias <- getInt16be 14
    pure GlonassCodeBias {..}

  putBits _n GlonassCodeBias {..} = do
    B.putWord8 5  _glonassCodeBias_signal
    putInt16be 14 _glonassCodeBias_codeBias

-- | GlonassCodeBiasCorrectionMessage.
--
-- GLONASS code bias correction message.
data GlonassCodeBiasCorrection = GlonassCodeBiasCorrection
  { _glonassCodeBiasCorrection_sat        :: Word8
    -- ^ GLONASS satellite id.
  , _glonassCodeBiasCorrection_n          :: Word8
    -- ^ Number of biases.
  , _glonassCodeBiasCorrection_codeBiases :: [GlonassCodeBias]
    -- ^ GLONASS code biases.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GlonassCodeBiasCorrection)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_glonassCodeBiasCorrection_" . stripPrefix "_glonassCodeBiasCorrection_"} ''GlonassCodeBiasCorrection)

instance BinaryBit GlonassCodeBiasCorrection where
  getBits _n = do
    _glonassCodeBiasCorrection_sat        <- B.getWord8 5
    _glonassCodeBiasCorrection_n          <- B.getWord8 5
    _glonassCodeBiasCorrection_codeBiases <- replicateM (fromIntegral _glonassCodeBiasCorrection_n) $ getBits 0
    pure GlonassCodeBiasCorrection {..}

  putBits _n GlonassCodeBiasCorrection {..} = do
    B.putWord8 5 _glonassCodeBiasCorrection_sat
    B.putWord8 5 _glonassCodeBiasCorrection_n
    forM_ _glonassCodeBiasCorrection_codeBiases $ putBits 0

-- | GpsPhaseBiasCorrectionHeader.
--
-- GPS phase bias correction header.
data GpsPhaseBiasCorrectionHeader = GpsPhaseBiasCorrectionHeader
  { _gpsPhaseBiasCorrectionHeader_num            :: Word16
    -- ^ Message number.
  , _gpsPhaseBiasCorrectionHeader_epochs         :: Word32
    -- ^ GPS epoch time.
  , _gpsPhaseBiasCorrectionHeader_updateInterval :: Word8
    -- ^ SSR update interval.
  , _gpsPhaseBiasCorrectionHeader_multiple       :: Bool
    -- ^ Multiple message indicator.
  , _gpsPhaseBiasCorrectionHeader_iod            :: Word8
    -- ^ IOD SSR.
  , _gpsPhaseBiasCorrectionHeader_provider       :: Word16
    -- ^ SSR provider id.
  , _gpsPhaseBiasCorrectionHeader_solution       :: Word8
    -- ^ SSR solution id.
  , _gpsPhaseBiasCorrectionHeader_dispersive     :: Bool
    -- ^ Dispersive Bias Consistency Indicator.
  , _gpsPhaseBiasCorrectionHeader_mw             :: Bool
    -- ^ MW Consistency Indicator.
  , _gpsPhaseBiasCorrectionHeader_n              :: Word8
    -- ^ Number of satellites.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsPhaseBiasCorrectionHeader)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_gpsPhaseBiasCorrectionHeader_" . stripPrefix "_gpsPhaseBiasCorrectionHeader_"} ''GpsPhaseBiasCorrectionHeader)

instance BinaryBit GpsPhaseBiasCorrectionHeader where
  getBits _n = do
    _gpsPhaseBiasCorrectionHeader_num            <- B.getWord16be 12
    _gpsPhaseBiasCorrectionHeader_epochs         <- B.getWord32be 20
    _gpsPhaseBiasCorrectionHeader_updateInterval <- B.getWord8 4
    _gpsPhaseBiasCorrectionHeader_multiple       <- B.getBool
    _gpsPhaseBiasCorrectionHeader_iod            <- B.getWord8 4
    _gpsPhaseBiasCorrectionHeader_provider       <- B.getWord16be 16
    _gpsPhaseBiasCorrectionHeader_solution       <- B.getWord8 4
    _gpsPhaseBiasCorrectionHeader_dispersive     <- B.getBool
    _gpsPhaseBiasCorrectionHeader_mw             <- B.getBool
    _gpsPhaseBiasCorrectionHeader_n              <- B.getWord8 6
    pure GpsPhaseBiasCorrectionHeader {..}

  putBits _n GpsPhaseBiasCorrectionHeader {..} = do
    B.putWord16be 12 _gpsPhaseBiasCorrectionHeader_num
    B.putWord32be 20 _gpsPhaseBiasCorrectionHeader_epochs
    B.putWord8 4     _gpsPhaseBiasCorrectionHeader_updateInterval
    B.putBool        _gpsPhaseBiasCorrectionHeader_multiple
    B.putWord8 4     _gpsPhaseBiasCorrectionHeader_iod
    B.putWord16be 16 _gpsPhaseBiasCorrectionHeader_provider
    B.putWord8 4     _gpsPhaseBiasCorrectionHeader_solution
    B.putBool        _gpsPhaseBiasCorrectionHeader_dispersive
    B.putBool        _gpsPhaseBiasCorrectionHeader_mw
    B.putWord8 6     _gpsPhaseBiasCorrectionHeader_n

-- | GpsPhaseBias.
--
-- GPS phase bias.
data GpsPhaseBias = GpsPhaseBias
  { _gpsPhaseBias_signal               :: Word8
    -- ^ GPS signal.
  , _gpsPhaseBias_integer              :: Bool
    -- ^ Signal Integer Indicator.
  , _gpsPhaseBias_wideLaneInteger      :: Word8
    -- ^ Signals wide-lane integer indicator.
  , _gpsPhaseBias_discontinuityCounter :: Word8
    -- ^ Signal Discontinuity Counter.
  , _gpsPhaseBias_phaseBias            :: Int32
    -- ^ GPS phase bias.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsPhaseBias)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_gpsPhaseBias_" . stripPrefix "_gpsPhaseBias_"} ''GpsPhaseBias)

instance BinaryBit GpsPhaseBias where
  getBits _n = do
    _gpsPhaseBias_signal               <- B.getWord8 5
    _gpsPhaseBias_integer              <- B.getBool
    _gpsPhaseBias_wideLaneInteger      <- B.getWord8 2
    _gpsPhaseBias_discontinuityCounter <- B.getWord8 4
    _gpsPhaseBias_phaseBias            <- getInt32be 20
    pure GpsPhaseBias {..}

  putBits _n GpsPhaseBias {..} = do
    B.putWord8 5  _gpsPhaseBias_signal
    B.putBool     _gpsPhaseBias_integer
    B.putWord8 2  _gpsPhaseBias_wideLaneInteger
    B.putWord8 4  _gpsPhaseBias_discontinuityCounter
    putInt32be 20 _gpsPhaseBias_phaseBias

-- | GpsPhaseBiasCorrectionMessage.
--
-- GPS phase bias correction message.
data GpsPhaseBiasCorrection = GpsPhaseBiasCorrection
  { _gpsPhaseBiasCorrection_sat        :: Word8
    -- ^ GPS satellite id.
  , _gpsPhaseBiasCorrection_n          :: Word8
    -- ^ Number of biases.
  , _gpsPhaseBiasCorrection_yawAngle   :: Word16
    -- ^ Yaw angle.
  , _gpsPhaseBiasCorrection_yawRate    :: Int8
    -- ^ Yaw rate.
  , _gpsPhaseBiasCorrection_phaseBiases :: [GpsPhaseBias]
    -- ^ GPS phase biases.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsPhaseBiasCorrection)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_gpsPhaseBiasCorrection_" . stripPrefix "_gpsPhaseBiasCorrection_"} ''GpsPhaseBiasCorrection)

instance BinaryBit GpsPhaseBiasCorrection where
  getBits _n = do
    _gpsPhaseBiasCorrection_sat         <- B.getWord8 6
    _gpsPhaseBiasCorrection_n           <- B.getWord8 5
    _gpsPhaseBiasCorrection_yawAngle    <- B.getWord16be 9
    _gpsPhaseBiasCorrection_yawRate     <- getInt8 8
    _gpsPhaseBiasCorrection_phaseBiases <- replicateM (fromIntegral _gpsPhaseBiasCorrection_n) $ getBits 0
    pure GpsPhaseBiasCorrection {..}

  putBits _n GpsPhaseBiasCorrection {..} = do
    B.putWord8 6    _gpsPhaseBiasCorrection_sat
    B.putWord8 5    _gpsPhaseBiasCorrection_n
    B.putWord16be 9 _gpsPhaseBiasCorrection_yawAngle
    putInt8 8       _gpsPhaseBiasCorrection_yawRate
    forM_ _gpsPhaseBiasCorrection_phaseBiases $ putBits 0

-- | GlonassPhaseBiasCorrectionHeader.
--
-- GLONASS phase bias correction header.
data GlonassPhaseBiasCorrectionHeader = GlonassPhaseBiasCorrectionHeader
  { _glonassPhaseBiasCorrectionHeader_num            :: Word16
    -- ^ Message number.
  , _glonassPhaseBiasCorrectionHeader_epochs         :: Word32
    -- ^ GLONASS epoch time.
  , _glonassPhaseBiasCorrectionHeader_updateInterval :: Word8
    -- ^ SSR update interval.
  , _glonassPhaseBiasCorrectionHeader_multiple       :: Bool
    -- ^ Multiple message indicator.
  , _glonassPhaseBiasCorrectionHeader_iod            :: Word8
    -- ^ IOD SSR.
  , _glonassPhaseBiasCorrectionHeader_provider       :: Word16
    -- ^ SSR provider id.
  , _glonassPhaseBiasCorrectionHeader_solution       :: Word8
    -- ^ SSR solution id.
  , _glonassPhaseBiasCorrectionHeader_dispersive     :: Bool
    -- ^ Dispersive Bias Consistency Indicator.
  , _glonassPhaseBiasCorrectionHeader_mw             :: Bool
    -- ^ MW Consistency Indicator.
  , _glonassPhaseBiasCorrectionHeader_n              :: Word8
    -- ^ Number of satellites.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GlonassPhaseBiasCorrectionHeader)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_glonassPhaseBiasCorrectionHeader_" . stripPrefix "_glonassPhaseBiasCorrectionHeader_"} ''GlonassPhaseBiasCorrectionHeader)

instance BinaryBit GlonassPhaseBiasCorrectionHeader where
  getBits _n = do
    _glonassPhaseBiasCorrectionHeader_num            <- B.getWord16be 12
    _glonassPhaseBiasCorrectionHeader_epochs         <- B.getWord32be 17
    _glonassPhaseBiasCorrectionHeader_updateInterval <- B.getWord8 4
    _glonassPhaseBiasCorrectionHeader_multiple       <- B.getBool
    _glonassPhaseBiasCorrectionHeader_iod            <- B.getWord8 4
    _glonassPhaseBiasCorrectionHeader_provider       <- B.getWord16be 16
    _glonassPhaseBiasCorrectionHeader_solution       <- B.getWord8 4
    _glonassPhaseBiasCorrectionHeader_dispersive     <- B.getBool
    _glonassPhaseBiasCorrectionHeader_mw             <- B.getBool
    _glonassPhaseBiasCorrectionHeader_n              <- B.getWord8 6
    pure GlonassPhaseBiasCorrectionHeader {..}

  putBits _n GlonassPhaseBiasCorrectionHeader {..} = do
    B.putWord16be 12 _glonassPhaseBiasCorrectionHeader_num
    B.putWord32be 17 _glonassPhaseBiasCorrectionHeader_epochs
    B.putWord8 4     _glonassPhaseBiasCorrectionHeader_updateInterval
    B.putBool        _glonassPhaseBiasCorrectionHeader_multiple
    B.putWord8 4     _glonassPhaseBiasCorrectionHeader_iod
    B.putWord16be 16 _glonassPhaseBiasCorrectionHeader_provider
    B.putWord8 4     _glonassPhaseBiasCorrectionHeader_solution
    B.putBool        _glonassPhaseBiasCorrectionHeader_dispersive
    B.putBool        _glonassPhaseBiasCorrectionHeader_mw
    B.putWord8 6     _glonassPhaseBiasCorrectionHeader_n

-- | GlonassPhaseBias.
--
-- GLONASS phase bias.
data GlonassPhaseBias = GlonassPhaseBias
  { _glonassPhaseBias_signal               :: Word8
    -- ^ GLONASS signal.
  , _glonassPhaseBias_integer              :: Bool
    -- ^ Signal Integer Indicator.
  , _glonassPhaseBias_wideLaneInteger      :: Word8
    -- ^ Signals wide-lane integer indicator.
  , _glonassPhaseBias_discontinuityCounter :: Word8
    -- ^ Signal Discontinuity Counter.
  , _glonassPhaseBias_phaseBias            :: Int32
    -- ^ GLONASS phase bias.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GlonassPhaseBias)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_glonassPhaseBias_" . stripPrefix "_glonassPhaseBias_"} ''GlonassPhaseBias)

instance BinaryBit GlonassPhaseBias where
  getBits _n = do
    _glonassPhaseBias_signal               <- B.getWord8 5
    _glonassPhaseBias_integer              <- B.getBool
    _glonassPhaseBias_wideLaneInteger      <- B.getWord8 2
    _glonassPhaseBias_discontinuityCounter <- B.getWord8 4
    _glonassPhaseBias_phaseBias            <- getInt32be 20
    pure GlonassPhaseBias {..}

  putBits _n GlonassPhaseBias {..} = do
    B.putWord8 5  _glonassPhaseBias_signal
    B.putBool     _glonassPhaseBias_integer
    B.putWord8 2  _glonassPhaseBias_wideLaneInteger
    B.putWord8 4  _glonassPhaseBias_discontinuityCounter
    putInt32be 20 _glonassPhaseBias_phaseBias

-- | GlonassPhaseBiasCorrectionMessage.
--
-- GLONASS phase bias correction message.
data GlonassPhaseBiasCorrection = GlonassPhaseBiasCorrection
  { _glonassPhaseBiasCorrection_sat        :: Word8
    -- ^ GLONASS satellite id.
  , _glonassPhaseBiasCorrection_n          :: Word8
    -- ^ Number of biases.
  , _glonassPhaseBiasCorrection_yawAngle   :: Word16
    -- ^ Yaw angle.
  , _glonassPhaseBiasCorrection_yawRate    :: Int8
    -- ^ Yaw rate.
  , _glonassPhaseBiasCorrection_phaseBiases :: [GlonassPhaseBias]
    -- ^ GLONASS phase biases.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GlonassPhaseBiasCorrection)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_glonassPhaseBiasCorrection_" . stripPrefix "_glonassPhaseBiasCorrection_"} ''GlonassPhaseBiasCorrection)

instance BinaryBit GlonassPhaseBiasCorrection where
  getBits _n = do
    _glonassPhaseBiasCorrection_sat         <- B.getWord8 5
    _glonassPhaseBiasCorrection_n           <- B.getWord8 5
    _glonassPhaseBiasCorrection_yawAngle    <- B.getWord16be 9
    _glonassPhaseBiasCorrection_yawRate     <- getInt8 8
    _glonassPhaseBiasCorrection_phaseBiases <- replicateM (fromIntegral _glonassPhaseBiasCorrection_n) $ getBits 0
    pure GlonassPhaseBiasCorrection {..}

  putBits _n GlonassPhaseBiasCorrection {..} = do
    B.putWord8 5    _glonassPhaseBiasCorrection_sat
    B.putWord8 5    _glonassPhaseBiasCorrection_n
    B.putWord16be 9 _glonassPhaseBiasCorrection_yawAngle
    putInt8 8       _glonassPhaseBiasCorrection_yawRate
    forM_ _glonassPhaseBiasCorrection_phaseBiases $ putBits 0

msg1057 :: Word16
msg1057 = 1057

-- | Msg 1057.
--
-- RTCMv3 message 1057.
data Msg1057 = Msg1057
  { _msg1057_header      :: GpsOrbitCorrectionHeader
    -- ^ GPS orbit correction header.
  , _msg1057_corrections :: [GpsOrbitCorrection]
    -- ^ GPS orbit corrections.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1057)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1057_" . stripPrefix "_msg1057_"} ''Msg1057)

instance Binary Msg1057 where
  get = B.runBitGet $ do
    _msg1057_header      <- getBits 0
    _msg1057_corrections <- replicateM (fromIntegral $ _msg1057_header ^. gpsOrbitCorrectionHeader_n) $ getBits 0
    pure Msg1057 {..}

  put Msg1057 {..} = B.runBitPut $ do
    putBits 0 _msg1057_header
    forM_ _msg1057_corrections $ putBits 0

$(deriveRTCM3 ''Msg1057)

msg1058 :: Word16
msg1058 = 1058

-- | Msg 1058.
--
-- RTCMv3 message 1058.
data Msg1058 = Msg1058
  { _msg1058_header      :: GpsClockCorrectionHeader
    -- ^ GPS clock correction header.
  , _msg1058_corrections :: [GpsClockCorrection]
    -- ^ GPS clock corrections.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1058)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1058_" . stripPrefix "_msg1058_"} ''Msg1058)

instance Binary Msg1058 where
  get = B.runBitGet $ do
    _msg1058_header      <- getBits 0
    _msg1058_corrections <- replicateM (fromIntegral $ _msg1058_header ^. gpsClockCorrectionHeader_n) $ getBits 0
    pure Msg1058 {..}

  put Msg1058 {..} = B.runBitPut $ do
    putBits 0 _msg1058_header
    forM_ _msg1058_corrections $ putBits 0

$(deriveRTCM3 ''Msg1058)

msg1063 :: Word16
msg1063 = 1063

-- | Msg 1063.
--
-- RTCMv3 message 1063.
data Msg1063 = Msg1063
  { _msg1063_header      :: GlonassOrbitCorrectionHeader
    -- ^ GLONASS orbit correction header.
  , _msg1063_corrections :: [GlonassOrbitCorrection]
    -- ^ GLONASS orbit corrections.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1063)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1063_" . stripPrefix "_msg1063_"} ''Msg1063)

instance Binary Msg1063 where
  get = B.runBitGet $ do
    _msg1063_header      <- getBits 0
    _msg1063_corrections <- replicateM (fromIntegral $ _msg1063_header ^. glonassOrbitCorrectionHeader_n) $ getBits 0
    pure Msg1063 {..}

  put Msg1063 {..} = B.runBitPut $ do
    putBits 0 _msg1063_header
    forM_ _msg1063_corrections $ putBits 0

$(deriveRTCM3 ''Msg1063)

msg1064 :: Word16
msg1064 = 1064

-- | Msg 1064.
--
-- RTCMv3 message 1064.
data Msg1064 = Msg1064
  { _msg1064_header      :: GlonassClockCorrectionHeader
    -- ^ GLONASS clock correction header.
  , _msg1064_corrections :: [GlonassClockCorrection]
    -- ^ GLONASS clock corrections.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1064)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1064_" . stripPrefix "_msg1064_"} ''Msg1064)

instance Binary Msg1064 where
  get = B.runBitGet $ do
    _msg1064_header      <- getBits 0
    _msg1064_corrections <- replicateM (fromIntegral $ _msg1064_header ^. glonassClockCorrectionHeader_n) $ getBits 0
    pure Msg1064 {..}

  put Msg1064 {..} = B.runBitPut $ do
    putBits 0 _msg1064_header
    forM_ _msg1064_corrections $ putBits 0

$(deriveRTCM3 ''Msg1064)

msg1060 :: Word16
msg1060 = 1060

-- | Msg 1060.
--
-- RTCMv3 message 1060.
data Msg1060 = Msg1060
  { _msg1060_header      :: GpsOrbitClockCorrectionHeader
    -- ^ GPS orbit correction header.
  , _msg1060_corrections :: [GpsOrbitClockCorrection]
    -- ^ GPS orbit corrections.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1060)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1060_" . stripPrefix "_msg1060_"} ''Msg1060)

instance Binary Msg1060 where
  get = B.runBitGet $ do
    _msg1060_header      <- getBits 0
    _msg1060_corrections <- replicateM (fromIntegral $ _msg1060_header ^. gpsOrbitClockCorrectionHeader_n) $ getBits 0
    pure Msg1060 {..}

  put Msg1060 {..} = B.runBitPut $ do
    putBits 0 _msg1060_header
    forM_ _msg1060_corrections $ putBits 0

$(deriveRTCM3 ''Msg1060)

msg1066 :: Word16
msg1066 = 1066

-- | Msg 1066.
--
-- RTCMv3 message 1066.
data Msg1066 = Msg1066
  { _msg1066_header      :: GlonassOrbitClockCorrectionHeader
    -- ^ GLONASS orbit correction header.
  , _msg1066_corrections :: [GlonassOrbitClockCorrection]
    -- ^ GLONASS orbit corrections.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1066)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1066_" . stripPrefix "_msg1066_"} ''Msg1066)

instance Binary Msg1066 where
  get = B.runBitGet $ do
    _msg1066_header      <- getBits 0
    _msg1066_corrections <- replicateM (fromIntegral $ _msg1066_header ^. glonassOrbitClockCorrectionHeader_n) $ getBits 0
    pure Msg1066 {..}

  put Msg1066 {..} = B.runBitPut $ do
    putBits 0 _msg1066_header
    forM_ _msg1066_corrections $ putBits 0

$(deriveRTCM3 ''Msg1066)

msg1059 :: Word16
msg1059 = 1059

-- | Msg 1059.
--
-- RTCMv3 message 1059.
data Msg1059 = Msg1059
  { _msg1059_header      :: GpsCodeBiasCorrectionHeader
    -- ^ GPS code bias correction header.
  , _msg1059_corrections :: [GpsCodeBiasCorrection]
    -- ^ GPS code bias corrections.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1059)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1059_" . stripPrefix "_msg1059_"} ''Msg1059)

instance Binary Msg1059 where
  get = B.runBitGet $ do
    _msg1059_header      <- getBits 0
    _msg1059_corrections <- replicateM (fromIntegral $ _msg1059_header ^. gpsCodeBiasCorrectionHeader_n) $ getBits 0
    pure Msg1059 {..}

  put Msg1059 {..} = B.runBitPut $ do
    putBits 0 _msg1059_header
    forM_ _msg1059_corrections $ putBits 0

$(deriveRTCM3 ''Msg1059)

msg1065 :: Word16
msg1065 = 1065

-- | Msg 1065.
--
-- RTCMv3 message 1065.
data Msg1065 = Msg1065
  { _msg1065_header      :: GlonassCodeBiasCorrectionHeader
    -- ^ GLONASS code bias correction header.
  , _msg1065_corrections :: [GlonassCodeBiasCorrection]
    -- ^ GLONASS code bias corrections.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1065)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1065_" . stripPrefix "_msg1065_"} ''Msg1065)

instance Binary Msg1065 where
  get = B.runBitGet $ do
    _msg1065_header      <- getBits 0
    _msg1065_corrections <- replicateM (fromIntegral $ _msg1065_header ^. glonassCodeBiasCorrectionHeader_n) $ getBits 0
    pure Msg1065 {..}

  put Msg1065 {..} = B.runBitPut $ do
    putBits 0 _msg1065_header
    forM_ _msg1065_corrections $ putBits 0

$(deriveRTCM3 ''Msg1065)

msg1265 :: Word16
msg1265 = 1265

-- | Msg 1265.
--
-- RTCMv3 message 1265.
data Msg1265 = Msg1265
  { _msg1265_header      :: GpsPhaseBiasCorrectionHeader
    -- ^ GPS phase bias correction header.
  , _msg1265_corrections :: [GpsPhaseBiasCorrection]
    -- ^ GPS phase bias corrections.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1265)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1265_" . stripPrefix "_msg1265_"} ''Msg1265)

instance Binary Msg1265 where
  get = B.runBitGet $ do
    _msg1265_header      <- getBits 0
    _msg1265_corrections <- replicateM (fromIntegral $ _msg1265_header ^. gpsPhaseBiasCorrectionHeader_n) $ getBits 0
    pure Msg1265 {..}

  put Msg1265 {..} = B.runBitPut $ do
    putBits 0 _msg1265_header
    forM_ _msg1265_corrections $ putBits 0

$(deriveRTCM3 ''Msg1265)

msg1266 :: Word16
msg1266 = 1266

-- | Msg 1266.
--
-- RTCMv3 message 1266.
data Msg1266 = Msg1266
  { _msg1266_header      :: GlonassPhaseBiasCorrectionHeader
    -- ^ GLONASS phase bias correction header.
  , _msg1266_corrections :: [GlonassPhaseBiasCorrection]
    -- ^ GLONASS phase bias corrections.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1266)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1266_" . stripPrefix "_msg1266_"} ''Msg1266)

instance Binary Msg1266 where
  get = B.runBitGet $ do
    _msg1266_header      <- getBits 0
    _msg1266_corrections <- replicateM (fromIntegral $ _msg1266_header ^. glonassPhaseBiasCorrectionHeader_n) $ getBits 0
    pure Msg1266 {..}

  put Msg1266 {..} = B.runBitPut $ do
    putBits 0 _msg1266_header
    forM_ _msg1266_corrections $ putBits 0

$(deriveRTCM3 ''Msg1266)
