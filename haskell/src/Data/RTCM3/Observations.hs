{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module:      Data.RTCM3.Observations
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- RTCMv3 Observations.

module Data.RTCM3.Observations where

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

-- | GpsObservationHeader.
--
-- GPS RTK observation header.
data GpsObservationHeader = GpsObservationHeader
  { _gpsObservationHeader_num               :: Word16
    -- ^ Message number.
  , _gpsObservationHeader_station           :: Word16
    -- ^ Reference station id.
  , _gpsObservationHeader_tow               :: Word32
    -- ^ GPS epoch time.
  , _gpsObservationHeader_synchronous       :: Bool
    -- ^ Synchronous GNSS flag.
  , _gpsObservationHeader_n                 :: Word8
    -- ^ Number of GPS satellite observations.
  , _gpsObservationHeader_smoothing         :: Bool
    -- ^ GPS divergence-free smoothing indicator.
  , _gpsObservationHeader_smoothingInterval :: Word8
    -- ^ GPS smoothing interval.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsObservationHeader)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_gpsObservationHeader_" . stripPrefix "_gpsObservationHeader_"} ''GpsObservationHeader)

instance BinaryBit GpsObservationHeader where
  getBits _n = do
    _gpsObservationHeader_num               <- B.getWord16be 12
    _gpsObservationHeader_station           <- B.getWord16be 12
    _gpsObservationHeader_tow               <- B.getWord32be 30
    _gpsObservationHeader_synchronous       <- B.getBool
    _gpsObservationHeader_n                 <- B.getWord8 5
    _gpsObservationHeader_smoothing         <- B.getBool
    _gpsObservationHeader_smoothingInterval <- B.getWord8 3
    return GpsObservationHeader {..}

  putBits _n GpsObservationHeader {..} = do
    B.putWord16be 12 _gpsObservationHeader_num
    B.putWord16be 12 _gpsObservationHeader_station
    B.putWord32be 30 _gpsObservationHeader_tow
    B.putBool        _gpsObservationHeader_synchronous
    B.putWord8 5     _gpsObservationHeader_n
    B.putBool        _gpsObservationHeader_smoothing
    B.putWord8 3     _gpsObservationHeader_smoothingInterval

-- | GpsL1Observation.
--
-- GPS RTK L1 observation.
data GpsL1Observation = GpsL1Observation
  { _gpsL1Observation_code             :: Bool
    -- ^ GPS L1 code indicator.
  , _gpsL1Observation_pseudorange      :: Word32
    -- ^ GPS L1 pseudorange.
  , _gpsL1Observation_carrierMinusCode :: Int32
    -- ^ GPS L1 phaserange - pseudorange.
  , _gpsL1Observation_lockTime         :: Word8
    -- ^ GPS L1 lock time indicator.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsL1Observation)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_gpsL1Observation_" . stripPrefix "_gpsL1Observation_"} ''GpsL1Observation)

instance BinaryBit GpsL1Observation where
  getBits _n = do
    _gpsL1Observation_code             <- B.getBool
    _gpsL1Observation_pseudorange      <- B.getWord32be 24
    _gpsL1Observation_carrierMinusCode <- getInt32be 20
    _gpsL1Observation_lockTime         <- B.getWord8 7
    return GpsL1Observation {..}

  putBits _n GpsL1Observation {..} = do
    B.putBool        _gpsL1Observation_code
    B.putWord32be 24 _gpsL1Observation_pseudorange
    putInt32be 20    _gpsL1Observation_carrierMinusCode
    B.putWord8 7     _gpsL1Observation_lockTime

-- | GpsL1ExtObservation.
--
-- GPS RTK L1 extended observation.
data GpsL1ExtObservation = GpsL1ExtObservation
  { _gpsL1ExtObservation_ambiguity :: Word8
    -- ^ GPS L1 pseudorange ambiguity.
  , _gpsL1ExtObservation_cnr       :: Word8
    -- ^ GPS L1 carrier-to-noise ratio.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsL1ExtObservation)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_gpsL1ExtObservation_" . stripPrefix "_gpsL1ExtObservation_"} ''GpsL1ExtObservation)

instance BinaryBit GpsL1ExtObservation where
  getBits _n = do
    _gpsL1ExtObservation_ambiguity <- B.getWord8 8
    _gpsL1ExtObservation_cnr       <- B.getWord8 8
    return GpsL1ExtObservation {..}

  putBits _n GpsL1ExtObservation {..} = do
    B.putWord8 8 _gpsL1ExtObservation_ambiguity
    B.putWord8 8 _gpsL1ExtObservation_cnr

-- | GpsL2Observation.
--
-- GPS RTK L2 observation.
data GpsL2Observation = GpsL2Observation
  { _gpsL2Observation_code                  :: Word8
    -- ^ GPS L2 code indicator.
  , _gpsL2Observation_pseudorangeDifference :: Int16
    -- ^ GPS L2-L1 pseudorange difference.
  , _gpsL2Observation_carrierMinusCode      :: Int32
    -- ^ GPS L2 phaserange - L1 pseudorange.
  , _gpsL2Observation_lockTime              :: Word8
    -- ^ GPS L2 lock time indicator.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsL2Observation)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_gpsL2Observation_" . stripPrefix "_gpsL2Observation_"} ''GpsL2Observation)

instance BinaryBit GpsL2Observation where
  getBits _n = do
    _gpsL2Observation_code                  <- B.getWord8 2
    _gpsL2Observation_pseudorangeDifference <- getInt16be 14
    _gpsL2Observation_carrierMinusCode      <- getInt32be 20
    _gpsL2Observation_lockTime              <- B.getWord8 7
    return GpsL2Observation {..}

  putBits _n GpsL2Observation {..} = do
    B.putWord8 2  _gpsL2Observation_code
    putInt16be 14 _gpsL2Observation_pseudorangeDifference
    putInt32be 20 _gpsL2Observation_carrierMinusCode
    B.putWord8 7  _gpsL2Observation_lockTime

-- | GpsL2ExtObservation.
--
-- GPS RTK L2 extended observation.
newtype GpsL2ExtObservation = GpsL2ExtObservation
  { _gpsL2ExtObservation_cnr :: Word8
    -- ^ GPS L2 carrier-to-noise ratio.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsL2ExtObservation)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_gpsL2ExtObservation_" . stripPrefix "_gpsL2ExtObservation_"} ''GpsL2ExtObservation)

instance BinaryBit GpsL2ExtObservation where
  getBits _n = do
    _gpsL2ExtObservation_cnr <- B.getWord8 8
    return GpsL2ExtObservation {..}

  putBits _n GpsL2ExtObservation {..} =
    B.putWord8 8 _gpsL2ExtObservation_cnr

-- | GlonassObservationHeader.
--
-- GLONASS RTK observation header.
data GlonassObservationHeader = GlonassObservationHeader
  { _glonassObservationHeader_num               :: Word16
    -- ^ Message number.
  , _glonassObservationHeader_station           :: Word16
    -- ^ Reference station id.
  , _glonassObservationHeader_epoch             :: Word32
    -- ^ GLONASS epoch time.
  , _glonassObservationHeader_synchronous       :: Bool
    -- ^ Synchronous GNSS flag.
  , _glonassObservationHeader_n                 :: Word8
    -- ^ Number of GLONASS satellite observations.
  , _glonassObservationHeader_smoothing         :: Bool
    -- ^ GLONASS divergence-free smoothing indicator.
  , _glonassObservationHeader_smoothingInterval :: Word8
    -- ^ GLONASS smoothing interval.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GlonassObservationHeader)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_glonassObservationHeader_" . stripPrefix "_glonassObservationHeader_"} ''GlonassObservationHeader)

instance BinaryBit GlonassObservationHeader where
  getBits _n = do
    _glonassObservationHeader_num               <- B.getWord16be 12
    _glonassObservationHeader_station           <- B.getWord16be 12
    _glonassObservationHeader_epoch             <- B.getWord32be 27
    _glonassObservationHeader_synchronous       <- B.getBool
    _glonassObservationHeader_n                 <- B.getWord8 5
    _glonassObservationHeader_smoothing         <- B.getBool
    _glonassObservationHeader_smoothingInterval <- B.getWord8 3
    return GlonassObservationHeader {..}

  putBits _n GlonassObservationHeader {..} = do
    B.putWord16be 12 _glonassObservationHeader_num
    B.putWord16be 12 _glonassObservationHeader_station
    B.putWord32be 27 _glonassObservationHeader_epoch
    B.putBool        _glonassObservationHeader_synchronous
    B.putWord8 5     _glonassObservationHeader_n
    B.putBool        _glonassObservationHeader_smoothing
    B.putWord8 3     _glonassObservationHeader_smoothingInterval

-- | GlonassL1Observation.
--
-- GLONASS RTK L1 observation.
data GlonassL1Observation = GlonassL1Observation
  { _glonassL1Observation_code             :: Bool
    -- ^ GLONASS L1 code indicator.
  , _glonassL1Observation_frequency        :: Word8
    -- ^ GLONASS satellite frequency channel number.
  , _glonassL1Observation_pseudorange      :: Word32
    -- ^ GLONASS L1 pseudorange.
  , _glonassL1Observation_carrierMinusCode :: Int32
    -- ^ GLONASS L1 phaserange - pseudorange.
  , _glonassL1Observation_lockTime         :: Word8
    -- ^ GLONASS L1 lock time indicator.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GlonassL1Observation)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_glonassL1Observation_" . stripPrefix "_glonassL1Observation_"} ''GlonassL1Observation)

instance BinaryBit GlonassL1Observation where
  getBits _n = do
    _glonassL1Observation_code             <- B.getBool
    _glonassL1Observation_frequency        <- B.getWord8 5
    _glonassL1Observation_pseudorange      <- B.getWord32be 25
    _glonassL1Observation_carrierMinusCode <- getInt32be 20
    _glonassL1Observation_lockTime         <- B.getWord8 7
    return GlonassL1Observation {..}

  putBits _n GlonassL1Observation {..} = do
    B.putBool        _glonassL1Observation_code
    B.putWord8 5     _glonassL1Observation_frequency
    B.putWord32be 25 _glonassL1Observation_pseudorange
    putInt32be 20    _glonassL1Observation_carrierMinusCode
    B.putWord8 7     _glonassL1Observation_lockTime

-- | GlonassL1ExtObservation.
--
-- GLONASS RTK L1 extended observation.
data GlonassL1ExtObservation = GlonassL1ExtObservation
  { _glonassL1ExtObservation_ambiguity :: Word8
    -- ^ GLONASS L1 pseudorange ambiguity.
  , _glonassL1ExtObservation_cnr       :: Word8
    -- ^ GLONASS L1 carrier-to-noise ratio.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GlonassL1ExtObservation)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_glonassL1ExtObservation_" . stripPrefix "_glonassL1ExtObservation_"} ''GlonassL1ExtObservation)

instance BinaryBit GlonassL1ExtObservation where
  getBits _n = do
    _glonassL1ExtObservation_ambiguity <- B.getWord8 7
    _glonassL1ExtObservation_cnr       <- B.getWord8 8
    return GlonassL1ExtObservation {..}

  putBits _n GlonassL1ExtObservation {..} = do
    B.putWord8 7 _glonassL1ExtObservation_ambiguity
    B.putWord8 8 _glonassL1ExtObservation_cnr

-- | GlonassL2Observation.
--
-- GLONASS RTK L2 observation.
data GlonassL2Observation = GlonassL2Observation
  { _glonassL2Observation_code                  :: Word8
    -- ^ GLONASS L2 code indicator.
  , _glonassL2Observation_pseudorangeDifference :: Int16
    -- ^ GLONASS L2-L1 pseudorange difference.
  , _glonassL2Observation_carrierMinusCode      :: Int32
    -- ^ GLONASS L2 phaserange - L1 pseudorange.
  , _glonassL2Observation_lockTime              :: Word8
    -- ^ GLONASS L2 lock time indicator.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GlonassL2Observation)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_glonassL2Observation_" . stripPrefix "_glonassL2Observation_"} ''GlonassL2Observation)

instance BinaryBit GlonassL2Observation where
  getBits _n = do
    _glonassL2Observation_code                  <- B.getWord8 2
    _glonassL2Observation_pseudorangeDifference <- getInt16be 14
    _glonassL2Observation_carrierMinusCode      <- getInt32be 20
    _glonassL2Observation_lockTime              <- B.getWord8 7
    return GlonassL2Observation {..}

  putBits _n GlonassL2Observation {..} = do
    B.putWord8 2  _glonassL2Observation_code
    putInt16be 14 _glonassL2Observation_pseudorangeDifference
    putInt32be 20 _glonassL2Observation_carrierMinusCode
    B.putWord8 7  _glonassL2Observation_lockTime

-- | GlonassL2ExtObservation.
--
-- GLONASS RTK L2 extended observation.
newtype GlonassL2ExtObservation = GlonassL2ExtObservation
  { _glonassL2ExtObservation_cnr :: Word8
    -- ^ GLONASS L2 carrier-to-noise ratio.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GlonassL2ExtObservation)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_glonassL2ExtObservation_" . stripPrefix "_glonassL2ExtObservation_"} ''GlonassL2ExtObservation)

instance BinaryBit GlonassL2ExtObservation where
  getBits _n = do
    _glonassL2ExtObservation_cnr <- B.getWord8 8
    return GlonassL2ExtObservation {..}

  putBits _n GlonassL2ExtObservation {..} =
    B.putWord8 8 _glonassL2ExtObservation_cnr

-- | GlonassBias.
--
-- GLONASS bias information.
data GlonassBias = GlonassBias
  { _glonassBias_num     :: Word16
    -- ^ Message number.
  , _glonassBias_station :: Word16
    -- ^ Reference station id.
  , _glonassBias_bias    :: Bool
    -- ^ GLONASS Code-Phase bias indicator.
  , _glonassBias_mask    :: Word8
    -- ^ GLONASS FDMA signals mask.
  , _glonassBias_l1ca    :: Word16
    -- ^ GLONASS L1 C/A code-phase bias.
  , _glonassBias_l1p     :: Word16
    -- ^ GLONASS L1 P code-phase bias.
  , _glonassBias_l2ca    :: Word16
    -- ^ GLONASS L2 C/A code-phase bias.
  , _glonassBias_l2p     :: Word16
    -- ^ GLONASS L2 P code-phase bias.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GlonassBias)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_glonassBias_" . stripPrefix "_glonassBias_"} ''GlonassBias)

instance BinaryBit GlonassBias where
  getBits _n = do
    _glonassBias_num     <- B.getWord16be 12
    _glonassBias_station <- B.getWord16be 12
    _glonassBias_bias    <- B.getBool
    _glonassBias_mask    <- B.getWord8 4
    _glonassBias_l1ca    <- B.getWord16be 16
    _glonassBias_l1p     <- B.getWord16be 16
    _glonassBias_l2ca    <- B.getWord16be 16
    _glonassBias_l2p     <- B.getWord16be 16
    return GlonassBias {..}

  putBits _n GlonassBias {..} = do
    B.putWord16be 12 _glonassBias_num
    B.putWord16be 12 _glonassBias_station
    B.putBool        _glonassBias_bias
    B.putWord8 4     _glonassBias_mask
    B.putWord16be 16 _glonassBias_l1ca
    B.putWord16be 16 _glonassBias_l1p
    B.putWord16be 16 _glonassBias_l2ca
    B.putWord16be 16 _glonassBias_l2p

msg1001 :: Word16
msg1001 = 1001

-- | Observation1001.
--
-- GPS RTK L1 observation for message 1001.
data Observation1001 = Observation1001
  { _observation1001_sat :: Word8
    -- ^ GPS satellite id.
  , _observation1001_l1  :: GpsL1Observation
    -- ^ GPS RTK L1 observation.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Observation1001)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_observation1001_" . stripPrefix "_observation1001_"} ''Observation1001)

instance BinaryBit Observation1001 where
  getBits n = do
    _observation1001_sat <- B.getWord8 6
    _observation1001_l1  <- getBits n
    return Observation1001 {..}

  putBits n Observation1001 {..} = do
    B.putWord8 6 _observation1001_sat
    putBits n _observation1001_l1

-- | Msg1001.
--
-- RTCMv3 message 1001.
data Msg1001 = Msg1001
  { _msg1001_header       :: GpsObservationHeader
    -- ^ GPS observation header.
  , _msg1001_observations :: [Observation1001]
    -- ^ GPS RTK L1 observations.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1001)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1001_" . stripPrefix "_msg1001_"} ''Msg1001)

instance Binary Msg1001 where
  get = B.runBitGet $ do
    _msg1001_header       <- getBits 0
    _msg1001_observations <- replicateM (fromIntegral $ _msg1001_header ^. gpsObservationHeader_n) $ getBits 0
    return Msg1001 {..}

  put Msg1001 {..} = B.runBitPut $ do
    putBits 0 _msg1001_header
    forM_ _msg1001_observations $ putBits 0

$(deriveRTCM3 ''Msg1001)

msg1002 :: Word16
msg1002 = 1002

-- | Observation1002.
--
-- GPS RTK L1 extended observation for message 1002.
data Observation1002 = Observation1002
  { _observation1002_sat :: Word8
    -- ^ GPS satellite id.
  , _observation1002_l1  :: GpsL1Observation
    -- ^ GPS RTK L1 observation.
  , _observation1002_l1e :: GpsL1ExtObservation
    -- ^ GPS RTK L1 extended observation.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Observation1002)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_observation1002_" . stripPrefix "_observation1002_"} ''Observation1002)

instance BinaryBit Observation1002 where
  getBits n = do
    _observation1002_sat <- B.getWord8 6
    _observation1002_l1  <- getBits n
    _observation1002_l1e <- getBits n
    return Observation1002 {..}

  putBits n Observation1002 {..} = do
    B.putWord8 6 _observation1002_sat
    putBits n _observation1002_l1
    putBits n _observation1002_l1e

-- | Msg1002.
--
-- RTCMv3 message 1002.
data Msg1002 = Msg1002
  { _msg1002_header       :: GpsObservationHeader
    -- ^ GPS observation header.
  , _msg1002_observations :: [Observation1002]
    -- ^ GPS RTK L1 extended observations.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1002)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1002_" . stripPrefix "_msg1002_"} ''Msg1002)

instance Binary Msg1002 where
  get = B.runBitGet $ do
    _msg1002_header       <- getBits 0
    _msg1002_observations <- replicateM (fromIntegral $ _msg1002_header ^. gpsObservationHeader_n) $ getBits 0
    return Msg1002 {..}

  put Msg1002 {..} = B.runBitPut $ do
    putBits 0 _msg1002_header
    forM_ _msg1002_observations $ putBits 0

$(deriveRTCM3 ''Msg1002)

msg1003 :: Word16
msg1003 = 1003

-- | Observation1003.
--
-- GPS RTK L1, L2 observation for message 1003.
data Observation1003 = Observation1003
  { _observation1003_sat :: Word8
    -- ^ GPS satellite id.
  , _observation1003_l1  :: GpsL1Observation
    -- ^ GPS RTK L1 observation.
  , _observation1003_l2  :: GpsL2Observation
    -- ^ GPS RTK L2 observation.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Observation1003)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_observation1003_" . stripPrefix "_observation1003_"} ''Observation1003)

instance BinaryBit Observation1003 where
  getBits n = do
    _observation1003_sat <- B.getWord8 6
    _observation1003_l1  <- getBits n
    _observation1003_l2  <- getBits n
    return Observation1003 {..}

  putBits n Observation1003 {..} = do
    B.putWord8 6 _observation1003_sat
    putBits n _observation1003_l1
    putBits n _observation1003_l2

-- | Msg1003.
--
-- RTCMv3 message 1003.
data Msg1003 = Msg1003
  { _msg1003_header       :: GpsObservationHeader
    -- ^ GPS observation header.
  , _msg1003_observations :: [Observation1003]
    -- ^ GPS RTK L1, L2 observations.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1003)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1003_" . stripPrefix "_msg1003_"} ''Msg1003)

instance Binary Msg1003 where
  get = B.runBitGet $ do
    _msg1003_header       <- getBits 0
    _msg1003_observations <- replicateM (fromIntegral $ _msg1003_header ^. gpsObservationHeader_n) $ getBits 0
    return Msg1003 {..}

  put Msg1003 {..} = B.runBitPut $ do
    putBits 0 _msg1003_header
    forM_ _msg1003_observations $ putBits 0

$(deriveRTCM3 ''Msg1003)

msg1004 :: Word16
msg1004 = 1004

-- | Observation1004.
--
-- GPS RTK L1, L2 extended observation for message 1004.
data Observation1004 = Observation1004
  { _observation1004_sat :: Word8
    -- ^ GPS satellite id.
  , _observation1004_l1  :: GpsL1Observation
    -- ^ GPS RTK L1 observation.
  , _observation1004_l1e :: GpsL1ExtObservation
    -- ^ GPS RTK L1 extended observation.
  , _observation1004_l2  :: GpsL2Observation
    -- ^ GPS RTK L2 observation.
  , _observation1004_l2e :: GpsL2ExtObservation
    -- ^ GPS RTK L2 extended observation.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Observation1004)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_observation1004_" . stripPrefix "_observation1004_"} ''Observation1004)

instance BinaryBit Observation1004 where
  getBits n = do
    _observation1004_sat <- B.getWord8 6
    _observation1004_l1  <- getBits n
    _observation1004_l1e <- getBits n
    _observation1004_l2  <- getBits n
    _observation1004_l2e <- getBits n
    return Observation1004 {..}

  putBits n Observation1004 {..} = do
    B.putWord8 6 _observation1004_sat
    putBits n _observation1004_l1
    putBits n _observation1004_l1e
    putBits n _observation1004_l2
    putBits n _observation1004_l2e

-- | Msg1004.
--
-- RTCMv3 message 1004.
data Msg1004 = Msg1004
  { _msg1004_header       :: GpsObservationHeader
    -- ^ GPS observation header.
  , _msg1004_observations :: [Observation1004]
    -- ^ GPS RTK L1, L2 extended observations.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1004)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1004_" . stripPrefix "_msg1004_"} ''Msg1004)

instance Binary Msg1004 where
  get = B.runBitGet $ do
    _msg1004_header       <- getBits 0
    _msg1004_observations <- replicateM (fromIntegral $ _msg1004_header ^. gpsObservationHeader_n) $ getBits 0
    return Msg1004 {..}

  put Msg1004 {..} = B.runBitPut $ do
    putBits 0 _msg1004_header
    forM_ _msg1004_observations $ putBits 0

$(deriveRTCM3 ''Msg1004)

msg1009 :: Word16
msg1009 = 1009

-- | Observation1009.
--
-- GLONASS RTK L1 observation for message 1009.
data Observation1009 = Observation1009
  { _observation1009_sat :: Word8
    -- ^ GLONASS satellite id.
  , _observation1009_l1  :: GlonassL1Observation
    -- ^ GLONASS RTK L1 observation.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Observation1009)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_observation1009_" . stripPrefix "_observation1009_"} ''Observation1009)

instance BinaryBit Observation1009 where
  getBits n = do
    _observation1009_sat <- B.getWord8 6
    _observation1009_l1  <- getBits n
    return Observation1009 {..}

  putBits n Observation1009 {..} = do
    B.putWord8 6 _observation1009_sat
    putBits n _observation1009_l1

-- | Msg1009.
--
-- RTCMv3 message 1009.
data Msg1009 = Msg1009
  { _msg1009_header       :: GlonassObservationHeader
    -- ^ GLONASS observation header.
  , _msg1009_observations :: [Observation1009]
    -- ^ GLONASS RTK L1 observations.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1009)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1009_" . stripPrefix "_msg1009_"} ''Msg1009)

instance Binary Msg1009 where
  get = B.runBitGet $ do
    _msg1009_header       <- getBits 0
    _msg1009_observations <- replicateM (fromIntegral $ _msg1009_header ^. glonassObservationHeader_n) $ getBits 0
    return Msg1009 {..}

  put Msg1009 {..} = B.runBitPut $ do
    putBits 0 _msg1009_header
    forM_ _msg1009_observations $ putBits 0

$(deriveRTCM3 ''Msg1009)

msg1010 :: Word16
msg1010 = 1010

-- | Observation1010.
--
-- GLONASS RTK L1 extended observation for message 1010.
data Observation1010 = Observation1010
  { _observation1010_sat :: Word8
    -- ^ GLONASS satellite id.
  , _observation1010_l1  :: GlonassL1Observation
    -- ^ GLONASS RTK L1 observation.
  , _observation1010_l1e :: GlonassL1ExtObservation
    -- ^ GLONASS RTK L1 extended observation.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Observation1010)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_observation1010_" . stripPrefix "_observation1010_"} ''Observation1010)

instance BinaryBit Observation1010 where
  getBits n = do
    _observation1010_sat <- B.getWord8 6
    _observation1010_l1  <- getBits n
    _observation1010_l1e <- getBits n
    return Observation1010 {..}

  putBits n Observation1010 {..} = do
    B.putWord8 6 _observation1010_sat
    putBits n _observation1010_l1
    putBits n _observation1010_l1e

-- | Msg1010.
--
-- RTCMv3 message 1010.
data Msg1010 = Msg1010
  { _msg1010_header       :: GlonassObservationHeader
    -- ^ GLONASS observation header.
  , _msg1010_observations :: [Observation1010]
    -- ^ GLONASS RTK L1 extended observations.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1010)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1010_" . stripPrefix "_msg1010_"} ''Msg1010)

instance Binary Msg1010 where
  get = B.runBitGet $ do
    _msg1010_header       <- getBits 0
    _msg1010_observations <- replicateM (fromIntegral $ _msg1010_header ^. glonassObservationHeader_n) $ getBits 0
    return Msg1010 {..}

  put Msg1010 {..} = B.runBitPut $ do
    putBits 0 _msg1010_header
    forM_ _msg1010_observations $ putBits 0

$(deriveRTCM3 ''Msg1010)

msg1011 :: Word16
msg1011 = 1011

-- | Observation1011.
--
-- GLONASS RTK L1, L2 observation for message 1011.
data Observation1011 = Observation1011
  { _observation1011_sat :: Word8
    -- ^ GLONASS satellite id.
  , _observation1011_l1  :: GlonassL1Observation
    -- ^ GLONASS RTK L1 observation.
  , _observation1011_l2  :: GlonassL2Observation
    -- ^ GLONASS RTK L2 observation.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Observation1011)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_observation1011_" . stripPrefix "_observation1011_"} ''Observation1011)

instance BinaryBit Observation1011 where
  getBits n = do
    _observation1011_sat <- B.getWord8 6
    _observation1011_l1  <- getBits n
    _observation1011_l2  <- getBits n
    return Observation1011 {..}

  putBits n Observation1011 {..} = do
    B.putWord8 6 _observation1011_sat
    putBits n _observation1011_l1
    putBits n _observation1011_l2

-- | Msg1011.
--
-- RTCMv3 message 1011.
data Msg1011 = Msg1011
  { _msg1011_header       :: GlonassObservationHeader
    -- ^ GLONASS observation header.
  , _msg1011_observations :: [Observation1011]
    -- ^ GLONASS RTK L1, L2 observations.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1011)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1011_" . stripPrefix "_msg1011_"} ''Msg1011)

instance Binary Msg1011 where
  get = B.runBitGet $ do
    _msg1011_header       <- getBits 0
    _msg1011_observations <- replicateM (fromIntegral $ _msg1011_header ^. glonassObservationHeader_n) $ getBits 0
    return Msg1011 {..}

  put Msg1011 {..} = B.runBitPut $ do
    putBits 0 _msg1011_header
    forM_ _msg1011_observations $ putBits 0

$(deriveRTCM3 ''Msg1011)

msg1012 :: Word16
msg1012 = 1012

-- | Observation1012.
--
-- GLONASS RTK L1, L2 extended observation for message 1012.
data Observation1012 = Observation1012
  { _observation1012_sat :: Word8
    -- ^ GLONASS satellite id.
  , _observation1012_l1  :: GlonassL1Observation
    -- ^ GLONASS RTK L1 observation.
  , _observation1012_l1e :: GlonassL1ExtObservation
    -- ^ GLONASS RTK L1 extended observation.
  , _observation1012_l2  :: GlonassL2Observation
    -- ^ GLONASS RTK L2 observation.
  , _observation1012_l2e :: GlonassL2ExtObservation
    -- ^ GLONASS RTK L2 extended observation.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Observation1012)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_observation1012_" . stripPrefix "_observation1012_"} ''Observation1012)

instance BinaryBit Observation1012 where
  getBits n = do
    _observation1012_sat <- B.getWord8 6
    _observation1012_l1  <- getBits n
    _observation1012_l1e <- getBits n
    _observation1012_l2  <- getBits n
    _observation1012_l2e <- getBits n
    return Observation1012 {..}

  putBits n Observation1012 {..} = do
    B.putWord8 6 _observation1012_sat
    putBits n _observation1012_l1
    putBits n _observation1012_l1e
    putBits n _observation1012_l2
    putBits n _observation1012_l2e

-- | Msg1012.
--
-- RTCMv3 message 1012.
data Msg1012 = Msg1012
  { _msg1012_header       :: GlonassObservationHeader
    -- ^ GLONASS observation header.
  , _msg1012_observations :: [Observation1012]
    -- ^ GLONASS RTK L1, L2 extended observations.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1012)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1012_" . stripPrefix "_msg1012_"} ''Msg1012)

instance Binary Msg1012 where
  get = B.runBitGet $ do
    _msg1012_header       <- getBits 0
    _msg1012_observations <- replicateM (fromIntegral $ _msg1012_header ^. glonassObservationHeader_n) $ getBits 0
    return Msg1012 {..}

  put Msg1012 {..} = B.runBitPut $ do
    putBits 0 _msg1012_header
    forM_ _msg1012_observations $ putBits 0

$(deriveRTCM3 ''Msg1012)

msg1230 :: Word16
msg1230 = 1230

-- | Msg1230.
--
-- RTCMv3 message 1230.
newtype Msg1230 = Msg1230
  { _msg1230_bias :: GlonassBias
    -- ^ GLONASS bias.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1230)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1230_" . stripPrefix "_msg1230_"} ''Msg1230)

instance Binary Msg1230 where
  get = B.runBitGet $ do
    _msg1230_bias <- getBits 0
    return Msg1230 {..}

  put Msg1230 {..} = B.runBitPut $
    putBits 0 _msg1230_bias

$(deriveRTCM3 ''Msg1230)
