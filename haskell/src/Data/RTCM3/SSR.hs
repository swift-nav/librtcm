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

module Data.RTCM3.SSR where

import           BasicPrelude
import           Control.Lens
import           Data.Aeson.TH
import           Data.Binary
import           Data.Binary.Bits
import qualified Data.Binary.Bits.Get as B
import qualified Data.Binary.Bits.Put as B
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
    return GpsOrbitCorrectionHeader {..}

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
    return GpsOrbitCorrection {..}

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
    return GpsClockCorrectionHeader {..}

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
    return GpsClockCorrection {..}

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
    return GlonassOrbitCorrectionHeader {..}

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
    return GlonassOrbitCorrection {..}

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
    return GlonassClockCorrectionHeader {..}

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
    return GlonassClockCorrection {..}

  putBits _n GlonassClockCorrection {..} = do
    B.putWord8 5  _glonassClockCorrection_sat
    putInt32be 22 _glonassClockCorrection_deltaClockC0
    putInt32be 21 _glonassClockCorrection_deltaClockC1
    putInt32be 27 _glonassClockCorrection_deltaClockC2

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
    return Msg1057 {..}

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
    return Msg1058 {..}

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
    return Msg1063 {..}

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
    return Msg1064 {..}

  put Msg1064 {..} = B.runBitPut $ do
    putBits 0 _msg1064_header
    forM_ _msg1064_corrections $ putBits 0

$(deriveRTCM3 ''Msg1064)
