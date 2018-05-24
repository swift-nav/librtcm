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
import           Data.Int
import           Data.RTCM3.Extras
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

-- | Msm46SatelliteData.
--
-- MSM46 satellite data.
data Msm46SatelliteData = Msm46SatelliteData
  { _msm46SatelliteData_ranges       :: [Word8]
    -- ^ The number of integer milliseconds in GNSS Satellite rough ranges.
  , _msm46SatelliteData_rangesModulo :: [Word16]
    -- ^ GNSS Satellite rough ranges modulo 1 millisecond.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msm46SatelliteData)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msm46SatelliteData_" . stripPrefix "_msm46SatelliteData_"} ''Msm46SatelliteData)

-- | Get Bits for Msm4SatelliteData.
--
getBitsMsm46SatelliteData :: Int -> B.BitGet Msm46SatelliteData
getBitsMsm46SatelliteData n = do
  _msm46SatelliteData_ranges       <- replicateM n $ B.getWord8 8
  _msm46SatelliteData_rangesModulo <- replicateM n $ B.getWord16be 10
  pure Msm46SatelliteData {..}

-- | Put Bits for Msm46SatelliteData.
--
putBitsMsm46SatelliteData :: Msm46SatelliteData -> B.BitPut ()
putBitsMsm46SatelliteData Msm46SatelliteData {..} = do
  forM_ _msm46SatelliteData_ranges $ B.putWord8 8
  forM_ _msm46SatelliteData_rangesModulo $ B.putWord16be 10

-- | Msm57SatelliteData
--
-- MSM57 satellite data.
data Msm57SatelliteData = Msm57SatelliteData
  { _msm57SatelliteData_ranges          :: [Word8]
    -- ^ The number of integer milliseconds in GNSS Satellite rough ranges.
  , _msm57SatelliteData_extendeds       :: [Word8]
    -- ^ Extended satellite information.
  , _msm57SatelliteData_rangesModulo    :: [Word16]
    -- ^ GNSS Satellite rough ranges modulo 1 millisecond.
  , _msm57SatelliteData_phaseRangeRates :: [Word16]
    -- ^ GNSS Satellite rough PhaseRangeRates.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msm57SatelliteData)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msm57SatelliteData_" . stripPrefix "_msm57SatelliteData_"} ''Msm57SatelliteData)

-- | Get Bits for Msm57SatelliteData.
--
getBitsMsm57SatelliteData :: Int -> B.BitGet Msm57SatelliteData
getBitsMsm57SatelliteData n = do
  _msm57SatelliteData_ranges          <- replicateM n $ B.getWord8 8
  _msm57SatelliteData_extendeds       <- replicateM n $ B.getWord8 4
  _msm57SatelliteData_rangesModulo    <- replicateM n $ B.getWord16be 10
  _msm57SatelliteData_phaseRangeRates <- replicateM n $ B.getWord16be 14
  pure Msm57SatelliteData {..}

-- | Put Bits for Msm4SatelliteData.
--
putBitsMsm57SatelliteData :: Msm57SatelliteData -> B.BitPut ()
putBitsMsm57SatelliteData Msm57SatelliteData {..} = do
  forM_ _msm57SatelliteData_ranges $ B.putWord8 8
  forM_ _msm57SatelliteData_extendeds $ B.putWord8 4
  forM_ _msm57SatelliteData_rangesModulo $ B.putWord16be 10
  forM_ _msm57SatelliteData_phaseRangeRates $ B.putWord16be 14

-- | Msm4SignalData.
--
-- MSM4 signal data.
data Msm4SignalData = Msm4SignalData
  { _msm4SignalData_pseudoranges :: [Int16]
    -- ^ GNSS signal fine pseudoranges.
  , _msm4SignalData_phaseranges  :: [Int32]
    -- ^ GNSS signal fine phaserange data.
  , _msm4SignalData_lockTimes    :: [Word8]
    -- ^ GNSS phaserange lock time indicators.
  , _msm4SignalData_halfCycles   :: [Bool]
    -- ^ Half-cycle ambiguity indicators.
  , _msm4SignalData_cnrs         :: [Word8]
    -- ^ GNSS signal CNRs.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msm4SignalData)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msm4SignalData_" . stripPrefix "_msm4SignalData_"} ''Msm4SignalData)

-- | Get Bits for Msm4SignalData.
--
getBitsMsm4SignalData :: Int -> B.BitGet Msm4SignalData
getBitsMsm4SignalData n = do
  _msm4SignalData_pseudoranges <- replicateM n $ getInt16be 15
  _msm4SignalData_phaseranges  <- replicateM n $ getInt32be 22
  _msm4SignalData_lockTimes    <- replicateM n $ B.getWord8 4
  _msm4SignalData_halfCycles   <- replicateM n B.getBool
  _msm4SignalData_cnrs         <- replicateM n $ B.getWord8 6
  pure Msm4SignalData {..}

-- | Put Bits for Msm4SignalData.
--
putBitsMsm4SignalData :: Msm4SignalData -> B.BitPut ()
putBitsMsm4SignalData Msm4SignalData {..} = do
  forM_ _msm4SignalData_pseudoranges $ putInt16be 15
  forM_ _msm4SignalData_phaseranges $ putInt32be 22
  forM_ _msm4SignalData_lockTimes $ B.putWord8 4
  forM_ _msm4SignalData_halfCycles B.putBool
  forM_ _msm4SignalData_cnrs $ B.putWord8 6

-- | Msm5SignalData.
--
-- MSM5 signal data.
data Msm5SignalData = Msm5SignalData
  { _msm5SignalData_pseudoranges    :: [Int16]
    -- ^ GNSS signal fine pseudoranges.
  , _msm5SignalData_phaseranges     :: [Int32]
    -- ^ GNSS signal fine phaserange data.
  , _msm5SignalData_lockTimes       :: [Word8]
    -- ^ GNSS phaserange lock time indicators.
  , _msm5SignalData_halfCycles      :: [Bool]
    -- ^ Half-cycle ambiguity indicators.
  , _msm5SignalData_cnrs            :: [Word8]
    -- ^ GNSS signal CNRs.
  , _msm5SignalData_phaseRangeRates :: [Int16]
    -- ^ GNSS signal fine phaserangerates.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msm5SignalData)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msm5SignalData_" . stripPrefix "_msm5SignalData_"} ''Msm5SignalData)

-- | Get Bits for Msm5SignalData.
--
getBitsMsm5SignalData :: Int -> B.BitGet Msm5SignalData
getBitsMsm5SignalData n = do
  _msm5SignalData_pseudoranges    <- replicateM n $ getInt16be 15
  _msm5SignalData_phaseranges     <- replicateM n $ getInt32be 22
  _msm5SignalData_lockTimes       <- replicateM n $ B.getWord8 4
  _msm5SignalData_halfCycles      <- replicateM n B.getBool
  _msm5SignalData_cnrs            <- replicateM n $ B.getWord8 6
  _msm5SignalData_phaseRangeRates <- replicateM n $ getInt16be 15
  pure Msm5SignalData {..}

-- | Put Bits for Msm5SignalData.
--
putBitsMsm5SignalData :: Msm5SignalData -> B.BitPut ()
putBitsMsm5SignalData Msm5SignalData {..} = do
  forM_ _msm5SignalData_pseudoranges $ putInt16be 15
  forM_ _msm5SignalData_phaseranges $ putInt32be 22
  forM_ _msm5SignalData_lockTimes $ B.putWord8 4
  forM_ _msm5SignalData_halfCycles B.putBool
  forM_ _msm5SignalData_cnrs $ B.putWord8 6
  forM_ _msm5SignalData_phaseRangeRates $ putInt16be 15

-- | Msm6SignalData.
--
-- MSM6 signal data.
data Msm6SignalData = Msm6SignalData
  { _msm6SignalData_pseudoranges :: [Int32]
    -- ^ GNSS signal fine pseudoranges.
  , _msm6SignalData_phaseranges  :: [Int32]
    -- ^ GNSS signal fine phaserange data.
  , _msm6SignalData_lockTimes    :: [Word16]
    -- ^ GNSS phaserange lock time indicators.
  , _msm6SignalData_halfCycles   :: [Bool]
    -- ^ Half-cycle ambiguity indicators.
  , _msm6SignalData_cnrs         :: [Word16]
    -- ^ GNSS signal CNRs.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msm6SignalData)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msm6SignalData_" . stripPrefix "_msm6SignalData_"} ''Msm6SignalData)

-- | Get Bits for Msm6SignalData.
--
getBitsMsm6SignalData :: Int -> B.BitGet Msm6SignalData
getBitsMsm6SignalData n = do
  _msm6SignalData_pseudoranges <- replicateM n $ getInt32be 20
  _msm6SignalData_phaseranges  <- replicateM n $ getInt32be 24
  _msm6SignalData_lockTimes    <- replicateM n $ B.getWord16be 10
  _msm6SignalData_halfCycles   <- replicateM n B.getBool
  _msm6SignalData_cnrs         <- replicateM n $ B.getWord16be 10
  pure Msm6SignalData {..}

-- | Put Bits for Msm6SignalData.
--
putBitsMsm6SignalData :: Msm6SignalData -> B.BitPut ()
putBitsMsm6SignalData Msm6SignalData {..} = do
  forM_ _msm6SignalData_pseudoranges $ putInt32be 20
  forM_ _msm6SignalData_phaseranges $ putInt32be 24
  forM_ _msm6SignalData_lockTimes $ B.putWord16be 10
  forM_ _msm6SignalData_halfCycles B.putBool
  forM_ _msm6SignalData_cnrs $ B.putWord16be 10

-- | Msm7SignalData.
--
-- MSM7 signal data.
data Msm7SignalData = Msm7SignalData
  { _msm7SignalData_pseudoranges    :: [Int32]
    -- ^ GNSS signal fine pseudoranges.
  , _msm7SignalData_phaseranges     :: [Int32]
    -- ^ GNSS signal fine phaserange data.
  , _msm7SignalData_lockTimes       :: [Word16]
    -- ^ GNSS phaserange lock time indicators.
  , _msm7SignalData_halfCycles      :: [Bool]
    -- ^ Half-cycle ambiguity indicators.
  , _msm7SignalData_cnrs            :: [Word16]
    -- ^ GNSS signal CNRs.
  , _msm7SignalData_phaseRangeRates :: [Int16]
    -- ^ GNSS signal fine phaserangerates.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msm7SignalData)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msm7SignalData_" . stripPrefix "_msm7SignalData_"} ''Msm7SignalData)

-- | Get Bits for Msm7SignalData.
--
getBitsMsm7SignalData :: Int -> B.BitGet Msm7SignalData
getBitsMsm7SignalData n = do
  _msm7SignalData_pseudoranges    <- replicateM n $ getInt32be 20
  _msm7SignalData_phaseranges     <- replicateM n $ getInt32be 24
  _msm7SignalData_lockTimes       <- replicateM n $ B.getWord16be 10
  _msm7SignalData_halfCycles      <- replicateM n B.getBool
  _msm7SignalData_cnrs            <- replicateM n $ B.getWord16be 10
  _msm7SignalData_phaseRangeRates <- replicateM n $ getInt16be 15
  pure Msm7SignalData {..}

-- | Put Bits for Msm7SignalData.
--
putBitsMsm7SignalData :: Msm7SignalData -> B.BitPut ()
putBitsMsm7SignalData Msm7SignalData {..} = do
  forM_ _msm7SignalData_pseudoranges $ putInt32be 20
  forM_ _msm7SignalData_phaseranges $ putInt32be 24
  forM_ _msm7SignalData_lockTimes $ B.putWord16be 10
  forM_ _msm7SignalData_halfCycles B.putBool
  forM_ _msm7SignalData_cnrs $ B.putWord16be 10
  forM_ _msm7SignalData_phaseRangeRates $ putInt16be 15

msg1074 :: Word16
msg1074 = 1074

-- | Message 1074
--
-- RTCMv3 message 1074.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1074 = Msg1074
  { _msg1074_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1074_satelliteData :: Msm46SatelliteData
    -- ^ MSM satellite data.
  , _msg1074_signalData    :: Msm4SignalData
    -- ^ MSM signal data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1074)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1074_" . stripPrefix "_msg1074_"} ''Msg1074)

instance Binary Msg1074 where
  get = B.runBitGet $ do
    _msg1074_header        <- getBits 0
    _msg1074_satelliteData <- getBitsMsm46SatelliteData $ popCount $ _msg1074_header ^. msmHeader_satelliteMask
    _msg1074_signalData    <- getBitsMsm4SignalData $ popCount $ _msg1074_header ^. msmHeader_cellMask
    pure Msg1074 {..}

  put Msg1074 {..} = B.runBitPut $ do
    putBits 0                 _msg1074_header
    putBitsMsm46SatelliteData _msg1074_satelliteData
    putBitsMsm4SignalData     _msg1074_signalData

$(deriveRTCM3 ''Msg1074)

msg1075 :: Word16
msg1075 = 1075

-- | Message 1075
--
-- RTCMv3 message 1075.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1075 = Msg1075
  { _msg1075_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1075_satelliteData :: Msm57SatelliteData
    -- ^ MSM satellite data.
  , _msg1075_signalData    :: Msm5SignalData
    -- ^ MSM signal data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1075)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1075_" . stripPrefix "_msg1075_"} ''Msg1075)

instance Binary Msg1075 where
  get = B.runBitGet $ do
    _msg1075_header        <- getBits 0
    _msg1075_satelliteData <- getBitsMsm57SatelliteData $ popCount $ _msg1075_header ^. msmHeader_satelliteMask
    _msg1075_signalData    <- getBitsMsm5SignalData $ popCount $ _msg1075_header ^. msmHeader_cellMask
    pure Msg1075 {..}

  put Msg1075 {..} = B.runBitPut $ do
    putBits 0                 _msg1075_header
    putBitsMsm57SatelliteData _msg1075_satelliteData
    putBitsMsm5SignalData     _msg1075_signalData

$(deriveRTCM3 ''Msg1075)

msg1076 :: Word16
msg1076 = 1076

-- | Message 1076
--
-- RTCMv3 message 1076.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1076 = Msg1076
  { _msg1076_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1076_satelliteData :: Msm46SatelliteData
    -- ^ MSM satellite data.
  , _msg1076_signalData    :: Msm6SignalData
    -- ^ MSM signal data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1076)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1076_" . stripPrefix "_msg1076_"} ''Msg1076)

instance Binary Msg1076 where
  get = B.runBitGet $ do
    _msg1076_header        <- getBits 0
    _msg1076_satelliteData <- getBitsMsm46SatelliteData $ popCount $ _msg1076_header ^. msmHeader_satelliteMask
    _msg1076_signalData    <- getBitsMsm6SignalData $ popCount $ _msg1076_header ^. msmHeader_cellMask
    pure Msg1076 {..}

  put Msg1076 {..} = B.runBitPut $ do
    putBits 0                 _msg1076_header
    putBitsMsm46SatelliteData _msg1076_satelliteData
    putBitsMsm6SignalData     _msg1076_signalData

$(deriveRTCM3 ''Msg1076)

msg1077 :: Word16
msg1077 = 1077

-- | Message 1077
--
-- RTCMv3 message 1077.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1077 = Msg1077
  { _msg1077_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1077_satelliteData :: Msm57SatelliteData
    -- ^ MSM satellite data.
  , _msg1077_signalData    :: Msm7SignalData
    -- ^ MSM signal data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1077)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1077_" . stripPrefix "_msg1077_"} ''Msg1077)

instance Binary Msg1077 where
  get = B.runBitGet $ do
    _msg1077_header        <- getBits 0
    _msg1077_satelliteData <- getBitsMsm57SatelliteData $ popCount $ _msg1077_header ^. msmHeader_satelliteMask
    _msg1077_signalData    <- getBitsMsm7SignalData $ popCount $ _msg1077_header ^. msmHeader_cellMask
    pure Msg1077 {..}

  put Msg1077 {..} = B.runBitPut $ do
    putBits 0                 _msg1077_header
    putBitsMsm57SatelliteData _msg1077_satelliteData
    putBitsMsm7SignalData     _msg1077_signalData

$(deriveRTCM3 ''Msg1077)

msg1084 :: Word16
msg1084 = 1084

-- | Message 1084
--
-- RTCMv3 message 1084.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1084 = Msg1084
  { _msg1084_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1084_satelliteData :: Msm46SatelliteData
    -- ^ MSM satellite data.
  , _msg1084_signalData    :: Msm4SignalData
    -- ^ MSM satellite data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1084)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1084_" . stripPrefix "_msg1084_"} ''Msg1084)

instance Binary Msg1084 where
  get = B.runBitGet $ do
    _msg1084_header        <- getBits 0
    _msg1084_satelliteData <- getBitsMsm46SatelliteData $ popCount $ _msg1084_header ^. msmHeader_satelliteMask
    _msg1084_signalData    <- getBitsMsm4SignalData $ popCount $ _msg1084_header ^. msmHeader_cellMask
    pure Msg1084 {..}

  put Msg1084 {..} = B.runBitPut $ do
    putBits 0                 _msg1084_header
    putBitsMsm46SatelliteData _msg1084_satelliteData
    putBitsMsm4SignalData     _msg1084_signalData

$(deriveRTCM3 ''Msg1084)

msg1085 :: Word16
msg1085 = 1085

-- | Message 1085
--
-- RTCMv3 message 1085.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1085 = Msg1085
  { _msg1085_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1085_satelliteData :: Msm57SatelliteData
    -- ^ MSM satellite data.
  , _msg1085_signalData    :: Msm5SignalData
    -- ^ MSM signal data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1085)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1085_" . stripPrefix "_msg1085_"} ''Msg1085)

instance Binary Msg1085 where
  get = B.runBitGet $ do
    _msg1085_header <- getBits 0
    _msg1085_satelliteData <- getBitsMsm57SatelliteData $ popCount $ _msg1085_header ^. msmHeader_satelliteMask
    _msg1085_signalData    <- getBitsMsm5SignalData $ popCount $ _msg1085_header ^. msmHeader_cellMask
    pure Msg1085 {..}

  put Msg1085 {..} = B.runBitPut $ do
    putBits 0                 _msg1085_header
    putBitsMsm57SatelliteData _msg1085_satelliteData
    putBitsMsm5SignalData     _msg1085_signalData

$(deriveRTCM3 ''Msg1085)

msg1086 :: Word16
msg1086 = 1086

-- | Message 1086
--
-- RTCMv3 message 1086.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1086 = Msg1086
  { _msg1086_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1086_satelliteData :: Msm46SatelliteData
    -- ^ MSM satellite data.
  , _msg1086_signalData    :: Msm6SignalData
    -- ^ MSM signal data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1086)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1086_" . stripPrefix "_msg1086_"} ''Msg1086)

instance Binary Msg1086 where
  get = B.runBitGet $ do
    _msg1086_header        <- getBits 0
    _msg1086_satelliteData <- getBitsMsm46SatelliteData $ popCount $ _msg1086_header ^. msmHeader_satelliteMask
    _msg1086_signalData    <- getBitsMsm6SignalData $ popCount $ _msg1086_header ^. msmHeader_cellMask
    pure Msg1086 {..}

  put Msg1086 {..} = B.runBitPut $ do
    putBits 0                 _msg1086_header
    putBitsMsm46SatelliteData _msg1086_satelliteData
    putBitsMsm6SignalData     _msg1086_signalData

$(deriveRTCM3 ''Msg1086)

msg1087 :: Word16
msg1087 = 1087

-- | Message 1087
--
-- RTCMv3 message 1087.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1087 = Msg1087
  { _msg1087_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1087_satelliteData :: Msm57SatelliteData
    -- ^ MSM satellite data.
  , _msg1087_signalData    :: Msm7SignalData
    -- ^ MSM signal data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1087)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1087_" . stripPrefix "_msg1087_"} ''Msg1087)

instance Binary Msg1087 where
  get = B.runBitGet $ do
    _msg1087_header        <- getBits 0
    _msg1087_satelliteData <- getBitsMsm57SatelliteData $ popCount $ _msg1087_header ^. msmHeader_satelliteMask
    _msg1087_signalData    <- getBitsMsm7SignalData $ popCount $ _msg1087_header ^. msmHeader_cellMask
    pure Msg1087 {..}

  put Msg1087 {..} = B.runBitPut $ do
    putBits 0                 _msg1087_header
    putBitsMsm57SatelliteData _msg1087_satelliteData
    putBitsMsm7SignalData     _msg1087_signalData

$(deriveRTCM3 ''Msg1087)

msg1094 :: Word16
msg1094 = 1094

-- | Message 1094
--
-- RTCMv3 message 1094.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1094 = Msg1094
  { _msg1094_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1094_satelliteData :: Msm46SatelliteData
    -- ^ MSM satellite data.
  , _msg1094_signalData    :: Msm4SignalData
    -- ^ MSM satellite data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1094)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1094_" . stripPrefix "_msg1094_"} ''Msg1094)

instance Binary Msg1094 where
  get = B.runBitGet $ do
    _msg1094_header        <- getBits 0
    _msg1094_satelliteData <- getBitsMsm46SatelliteData $ popCount $ _msg1094_header ^. msmHeader_satelliteMask
    _msg1094_signalData    <- getBitsMsm4SignalData $ popCount $ _msg1094_header ^. msmHeader_cellMask
    pure Msg1094 {..}

  put Msg1094 {..} = B.runBitPut $ do
    putBits 0                 _msg1094_header
    putBitsMsm46SatelliteData _msg1094_satelliteData
    putBitsMsm4SignalData     _msg1094_signalData

$(deriveRTCM3 ''Msg1094)

msg1095 :: Word16
msg1095 = 1095

-- | Message 1095
--
-- RTCMv3 message 1095.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1095 = Msg1095
  { _msg1095_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1095_satelliteData :: Msm57SatelliteData
    -- ^ MSM satellite data.
  , _msg1095_signalData    :: Msm5SignalData
    -- ^ MSM signal data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1095)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1095_" . stripPrefix "_msg1095_"} ''Msg1095)

instance Binary Msg1095 where
  get = B.runBitGet $ do
    _msg1095_header        <- getBits 0
    _msg1095_satelliteData <- getBitsMsm57SatelliteData $ popCount $ _msg1095_header ^. msmHeader_satelliteMask
    _msg1095_signalData    <- getBitsMsm5SignalData $ popCount $ _msg1095_header ^. msmHeader_cellMask
    pure Msg1095 {..}

  put Msg1095 {..} = B.runBitPut $ do
    putBits 0                 _msg1095_header
    putBitsMsm57SatelliteData _msg1095_satelliteData
    putBitsMsm5SignalData     _msg1095_signalData

$(deriveRTCM3 ''Msg1095)

msg1096 :: Word16
msg1096 = 1096

-- | Message 1096
--
-- RTCMv3 message 1096.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1096 = Msg1096
  { _msg1096_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1096_satelliteData :: Msm46SatelliteData
    -- ^ MSM satellite data.
  , _msg1096_signalData    :: Msm6SignalData
    -- ^ MSM signal data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1096)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1096_" . stripPrefix "_msg1096_"} ''Msg1096)

instance Binary Msg1096 where
  get = B.runBitGet $ do
    _msg1096_header        <- getBits 0
    _msg1096_satelliteData <- getBitsMsm46SatelliteData $ popCount $ _msg1096_header ^. msmHeader_satelliteMask
    _msg1096_signalData    <- getBitsMsm6SignalData $ popCount $ _msg1096_header ^. msmHeader_cellMask
    pure Msg1096 {..}

  put Msg1096 {..} = B.runBitPut $ do
    putBits 0                 _msg1096_header
    putBitsMsm46SatelliteData _msg1096_satelliteData
    putBitsMsm6SignalData     _msg1096_signalData

$(deriveRTCM3 ''Msg1096)

msg1097 :: Word16
msg1097 = 1097

-- | Message 1097
--
-- RTCMv3 message 1097.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1097 = Msg1097
  { _msg1097_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1097_satelliteData :: Msm57SatelliteData
    -- ^ MSM satellite data.
  , _msg1097_signalData    :: Msm7SignalData
    -- ^ MSM signal data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1097)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1097_" . stripPrefix "_msg1097_"} ''Msg1097)

instance Binary Msg1097 where
  get = B.runBitGet $ do
    _msg1097_header        <- getBits 0
    _msg1097_satelliteData <- getBitsMsm57SatelliteData $ popCount $ _msg1097_header ^. msmHeader_satelliteMask
    _msg1097_signalData    <- getBitsMsm7SignalData $ popCount $ _msg1097_header ^. msmHeader_cellMask
    pure Msg1097 {..}

  put Msg1097 {..} = B.runBitPut $ do
    putBits 0                 _msg1097_header
    putBitsMsm57SatelliteData _msg1097_satelliteData
    putBitsMsm7SignalData     _msg1097_signalData

$(deriveRTCM3 ''Msg1097)

msg1104 :: Word16
msg1104 = 1104

-- | Message 1104
--
-- RTCMv3 message 1104.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1104 = Msg1104
  { _msg1104_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1104_satelliteData :: Msm46SatelliteData
    -- ^ MSM satellite data.
  , _msg1104_signalData    :: Msm4SignalData
    -- ^ MSM satellite data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1104)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1104_" . stripPrefix "_msg1104_"} ''Msg1104)

instance Binary Msg1104 where
  get = B.runBitGet $ do
    _msg1104_header        <- getBits 0
    _msg1104_satelliteData <- getBitsMsm46SatelliteData $ popCount $ _msg1104_header ^. msmHeader_satelliteMask
    _msg1104_signalData    <- getBitsMsm4SignalData $ popCount $ _msg1104_header ^. msmHeader_cellMask
    pure Msg1104 {..}

  put Msg1104 {..} = B.runBitPut $ do
    putBits 0                 _msg1104_header
    putBitsMsm46SatelliteData _msg1104_satelliteData
    putBitsMsm4SignalData     _msg1104_signalData

$(deriveRTCM3 ''Msg1104)

msg1105 :: Word16
msg1105 = 1105

-- | Message 1105
--
-- RTCMv3 message 1105.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1105 = Msg1105
  { _msg1105_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1105_satelliteData :: Msm57SatelliteData
    -- ^ MSM satellite data.
  , _msg1105_signalData    :: Msm5SignalData
    -- ^ MSM signal data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1105)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1105_" . stripPrefix "_msg1105_"} ''Msg1105)

instance Binary Msg1105 where
  get = B.runBitGet $ do
    _msg1105_header        <- getBits 0
    _msg1105_satelliteData <- getBitsMsm57SatelliteData $ popCount $ _msg1105_header ^. msmHeader_satelliteMask
    _msg1105_signalData    <- getBitsMsm5SignalData $ popCount $ _msg1105_header ^. msmHeader_cellMask
    pure Msg1105 {..}

  put Msg1105 {..} = B.runBitPut $ do
    putBits 0                 _msg1105_header
    putBitsMsm57SatelliteData _msg1105_satelliteData
    putBitsMsm5SignalData     _msg1105_signalData

$(deriveRTCM3 ''Msg1105)

msg1106 :: Word16
msg1106 = 1106

-- | Message 1106
--
-- RTCMv3 message 1106.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1106 = Msg1106
  { _msg1106_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1106_satelliteData :: Msm46SatelliteData
    -- ^ MSM satellite data.
  , _msg1106_signalData    :: Msm6SignalData
    -- ^ MSM signal data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1106)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1106_" . stripPrefix "_msg1106_"} ''Msg1106)

instance Binary Msg1106 where
  get = B.runBitGet $ do
    _msg1106_header        <- getBits 0
    _msg1106_satelliteData <- getBitsMsm46SatelliteData $ popCount $ _msg1106_header ^. msmHeader_satelliteMask
    _msg1106_signalData    <- getBitsMsm6SignalData $ popCount $ _msg1106_header ^. msmHeader_cellMask
    pure Msg1106 {..}

  put Msg1106 {..} = B.runBitPut $ do
    putBits 0                 _msg1106_header
    putBitsMsm46SatelliteData _msg1106_satelliteData
    putBitsMsm6SignalData     _msg1106_signalData

$(deriveRTCM3 ''Msg1106)

msg1107 :: Word16
msg1107 = 1107

-- | Message 1107
--
-- RTCMv3 message 1107.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1107 = Msg1107
  { _msg1107_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1107_satelliteData :: Msm57SatelliteData
    -- ^ MSM satellite data.
  , _msg1107_signalData    :: Msm7SignalData
    -- ^ MSM signal data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1107)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1107_" . stripPrefix "_msg1107_"} ''Msg1107)

instance Binary Msg1107 where
  get = B.runBitGet $ do
    _msg1107_header        <- getBits 0
    _msg1107_satelliteData <- getBitsMsm57SatelliteData $ popCount $ _msg1107_header ^. msmHeader_satelliteMask
    _msg1107_signalData    <- getBitsMsm7SignalData $ popCount $ _msg1107_header ^. msmHeader_cellMask
    pure Msg1107 {..}

  put Msg1107 {..} = B.runBitPut $ do
    putBits 0                 _msg1107_header
    putBitsMsm57SatelliteData _msg1107_satelliteData
    putBitsMsm7SignalData     _msg1107_signalData

$(deriveRTCM3 ''Msg1107)

msg1114 :: Word16
msg1114 = 1114

-- | Message 1114
--
-- RTCMv3 message 1114.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1114 = Msg1114
  { _msg1114_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1114_satelliteData :: Msm46SatelliteData
    -- ^ MSM satellite data.
  , _msg1114_signalData    :: Msm4SignalData
    -- ^ MSM satellite data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1114)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1114_" . stripPrefix "_msg1114_"} ''Msg1114)

instance Binary Msg1114 where
  get = B.runBitGet $ do
    _msg1114_header        <- getBits 0
    _msg1114_satelliteData <- getBitsMsm46SatelliteData $ popCount $ _msg1114_header ^. msmHeader_satelliteMask
    _msg1114_signalData    <- getBitsMsm4SignalData $ popCount $ _msg1114_header ^. msmHeader_cellMask
    pure Msg1114 {..}

  put Msg1114 {..} = B.runBitPut $ do
    putBits 0                 _msg1114_header
    putBitsMsm46SatelliteData _msg1114_satelliteData
    putBitsMsm4SignalData     _msg1114_signalData

$(deriveRTCM3 ''Msg1114)

msg1115 :: Word16
msg1115 = 1115

-- | Message 1115
--
-- RTCMv3 message 1115.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1115 = Msg1115
  { _msg1115_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1115_satelliteData :: Msm57SatelliteData
    -- ^ MSM satellite data.
  , _msg1115_signalData    :: Msm5SignalData
    -- ^ MSM signal data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1115)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1115_" . stripPrefix "_msg1115_"} ''Msg1115)

instance Binary Msg1115 where
  get = B.runBitGet $ do
    _msg1115_header        <- getBits 0
    _msg1115_satelliteData <- getBitsMsm57SatelliteData $ popCount $ _msg1115_header ^. msmHeader_satelliteMask
    _msg1115_signalData    <- getBitsMsm5SignalData $ popCount $ _msg1115_header ^. msmHeader_cellMask
    pure Msg1115 {..}

  put Msg1115 {..} = B.runBitPut $ do
    putBits 0                 _msg1115_header
    putBitsMsm57SatelliteData _msg1115_satelliteData
    putBitsMsm5SignalData     _msg1115_signalData

$(deriveRTCM3 ''Msg1115)

msg1116 :: Word16
msg1116 = 1116

-- | Message 1116
--
-- RTCMv3 message 1116.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1116 = Msg1116
  { _msg1116_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1116_satelliteData :: Msm46SatelliteData
    -- ^ MSM satellite data.
  , _msg1116_signalData    :: Msm6SignalData
    -- ^ MSM signal data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1116)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1116_" . stripPrefix "_msg1116_"} ''Msg1116)

instance Binary Msg1116 where
  get = B.runBitGet $ do
    _msg1116_header        <- getBits 0
    _msg1116_satelliteData <- getBitsMsm46SatelliteData $ popCount $ _msg1116_header ^. msmHeader_satelliteMask
    _msg1116_signalData    <- getBitsMsm6SignalData $ popCount $ _msg1116_header ^. msmHeader_cellMask
    pure Msg1116 {..}

  put Msg1116 {..} = B.runBitPut $ do
    putBits 0                 _msg1116_header
    putBitsMsm46SatelliteData _msg1116_satelliteData
    putBitsMsm6SignalData     _msg1116_signalData

$(deriveRTCM3 ''Msg1116)

msg1117 :: Word16
msg1117 = 1117

-- | Message 1117
--
-- RTCMv3 message 1117.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1117 = Msg1117
  { _msg1117_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1117_satelliteData :: Msm57SatelliteData
    -- ^ MSM satellite data.
  , _msg1117_signalData    :: Msm7SignalData
    -- ^ MSM signal data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1117)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1117_" . stripPrefix "_msg1117_"} ''Msg1117)

instance Binary Msg1117 where
  get = B.runBitGet $ do
    _msg1117_header        <- getBits 0
    _msg1117_satelliteData <- getBitsMsm57SatelliteData $ popCount $ _msg1117_header ^. msmHeader_satelliteMask
    _msg1117_signalData    <- getBitsMsm7SignalData $ popCount $ _msg1117_header ^. msmHeader_cellMask
    pure Msg1117 {..}

  put Msg1117 {..} = B.runBitPut $ do
    putBits 0                 _msg1117_header
    putBitsMsm57SatelliteData _msg1117_satelliteData
    putBitsMsm7SignalData     _msg1117_signalData

$(deriveRTCM3 ''Msg1117)

msg1124 :: Word16
msg1124 = 1124

-- | Message 1124
--
-- RTCMv3 message 1124.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1124 = Msg1124
  { _msg1124_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1124_satelliteData :: Msm46SatelliteData
    -- ^ MSM satellite data.
  , _msg1124_signalData    :: Msm4SignalData
    -- ^ MSM satellite data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1124)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1124_" . stripPrefix "_msg1124_"} ''Msg1124)

instance Binary Msg1124 where
  get = B.runBitGet $ do
    _msg1124_header        <- getBits 0
    _msg1124_satelliteData <- getBitsMsm46SatelliteData $ popCount $ _msg1124_header ^. msmHeader_satelliteMask
    _msg1124_signalData    <- getBitsMsm4SignalData $ popCount $ _msg1124_header ^. msmHeader_cellMask
    pure Msg1124 {..}

  put Msg1124 {..} = B.runBitPut $ do
    putBits 0                 _msg1124_header
    putBitsMsm46SatelliteData _msg1124_satelliteData
    putBitsMsm4SignalData     _msg1124_signalData

$(deriveRTCM3 ''Msg1124)

msg1125 :: Word16
msg1125 = 1125

-- | Message 1125
--
-- RTCMv3 message 1125.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1125 = Msg1125
  { _msg1125_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1125_satelliteData :: Msm57SatelliteData
    -- ^ MSM satellite data.
  , _msg1125_signalData    :: Msm5SignalData
    -- ^ MSM signal data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1125)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1125_" . stripPrefix "_msg1125_"} ''Msg1125)

instance Binary Msg1125 where
  get = B.runBitGet $ do
    _msg1125_header        <- getBits 0
    _msg1125_satelliteData <- getBitsMsm57SatelliteData $ popCount $ _msg1125_header ^. msmHeader_satelliteMask
    _msg1125_signalData    <- getBitsMsm5SignalData $ popCount $ _msg1125_header ^. msmHeader_cellMask
    pure Msg1125 {..}

  put Msg1125 {..} = B.runBitPut $ do
    putBits 0                 _msg1125_header
    putBitsMsm57SatelliteData _msg1125_satelliteData
    putBitsMsm5SignalData     _msg1125_signalData

$(deriveRTCM3 ''Msg1125)

msg1126 :: Word16
msg1126 = 1126

-- | Message 1126
--
-- RTCMv3 message 1126.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1126 = Msg1126
  { _msg1126_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1126_satelliteData :: Msm46SatelliteData
    -- ^ MSM satellite data.
  , _msg1126_signalData    :: Msm6SignalData
    -- ^ MSM signal data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1126)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1126_" . stripPrefix "_msg1126_"} ''Msg1126)

instance Binary Msg1126 where
  get = B.runBitGet $ do
    _msg1126_header        <- getBits 0
    _msg1126_satelliteData <- getBitsMsm46SatelliteData $ popCount $ _msg1126_header ^. msmHeader_satelliteMask
    _msg1126_signalData    <- getBitsMsm6SignalData $ popCount $ _msg1126_header ^. msmHeader_cellMask
    pure Msg1126 {..}

  put Msg1126 {..} = B.runBitPut $ do
    putBits 0                 _msg1126_header
    putBitsMsm46SatelliteData _msg1126_satelliteData
    putBitsMsm6SignalData     _msg1126_signalData

$(deriveRTCM3 ''Msg1126)

msg1127 :: Word16
msg1127 = 1127

-- | Message 1127
--
-- RTCMv3 message 1127.
--
-- See RTCM spec and MSM signal specification for more information
-- about these fields.
data Msg1127 = Msg1127
  { _msg1127_header        :: MsmHeader
    -- ^ MSM header.
  , _msg1127_satelliteData :: Msm57SatelliteData
    -- ^ MSM satellite data.
  , _msg1127_signalData    :: Msm7SignalData
    -- ^ MSM signal data.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1127)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1127_" . stripPrefix "_msg1127_"} ''Msg1127)

instance Binary Msg1127 where
  get = B.runBitGet $ do
    _msg1127_header        <- getBits 0
    _msg1127_satelliteData <- getBitsMsm57SatelliteData $ popCount $ _msg1127_header ^. msmHeader_satelliteMask
    _msg1127_signalData    <- getBitsMsm7SignalData $ popCount $ _msg1127_header ^. msmHeader_cellMask
    pure Msg1127 {..}

  put Msg1127 {..} = B.runBitPut $ do
    putBits 0                 _msg1127_header
    putBitsMsm57SatelliteData _msg1127_satelliteData
    putBitsMsm7SignalData     _msg1127_signalData

$(deriveRTCM3 ''Msg1127)

