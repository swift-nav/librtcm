{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module:      Data.RTCM3.Antennas
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- RTCMv3 antenna messages for stationary references and descriptors.

module Data.RTCM3.Antennas where

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

-- | AntennaReference.
--
-- Stationary antenna reference point information.
data AntennaReference = AntennaReference
  { _antennaReference_num          :: Word16
    -- ^ Message number.
  , _antennaReference_station      :: Word16
    -- ^ Reference station id.
  , _antennaReference_gps          :: Bool
    -- ^ GPS indicator.
  , _antennaReference_glonass      :: Bool
    -- ^ GLONASS indicator.
  , _antennaReference_computed     :: Bool
    -- ^ Reference-station non-physical indicator.
  , _antennaReference_ecef_x       :: Int64
    -- ^ Antenna reference point ECEF-X.
  , _antennaReference_oscillator   :: Bool
    -- ^ Single receiver oscillator indicator.
  , _antennaReference_ecef_y       :: Int64
    -- ^ Antenna reference point ECEF-Y.
  , _antennaReference_quarterCycle :: Word8
    -- ^ Quarter cycle indicator.
  , _antennaReference_ecef_z       :: Int64
    -- ^ Antenna reference point ECEF-Z.
  } deriving ( Show, Read, Eq )

$(makeLenses ''AntennaReference)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_antennaReference_" . stripPrefix "_antennaReference_"} ''AntennaReference)

instance BinaryBit AntennaReference where
  getBits _n = do
    _antennaReference_num          <- B.getWord16be 12
    _antennaReference_station      <- B.getWord16be 12
    _reserved                      <- B.getWord8 6
    _antennaReference_gps          <- B.getBool
    _antennaReference_glonass      <- B.getBool
    _reserved                      <- B.getBool
    _antennaReference_computed     <- B.getBool
    _antennaReference_ecef_x       <- getInt64be 38
    _antennaReference_oscillator   <- B.getBool
    _reserved                      <- B.getBool
    _antennaReference_ecef_y       <- getInt64be 38
    _antennaReference_quarterCycle <- B.getWord8 2
    _antennaReference_ecef_z       <- getInt64be 38
    return AntennaReference {..}

  putBits _n AntennaReference {..} = do
    B.putWord16be 12 _antennaReference_num
    B.putWord16be 12 _antennaReference_station
    B.putWord8 6     0
    B.putBool        _antennaReference_gps
    B.putBool        _antennaReference_glonass
    B.putBool        False
    B.putBool        _antennaReference_computed
    putInt64be 38    _antennaReference_ecef_x
    B.putBool        _antennaReference_oscillator
    B.putBool        False
    putInt64be 38    _antennaReference_ecef_y
    B.putWord8 2     _antennaReference_quarterCycle
    putInt64be 38    _antennaReference_ecef_z

-- | ExtAntennaReference.
--
-- Extended stationary antenna reference point information.
newtype ExtAntennaReference = ExtAntennaReference
  { _extAntennaReference_height :: Word16
    -- ^ Antenna height.
  } deriving ( Show, Read, Eq )

$(makeLenses ''ExtAntennaReference)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_extAntennaReference_" . stripPrefix "_extAntennaReference_"} ''ExtAntennaReference)

instance BinaryBit ExtAntennaReference where
  getBits _n = do
    _extAntennaReference_height <- B.getWord16be 16
    return ExtAntennaReference {..}

  putBits _n ExtAntennaReference {..} =
    B.putWord16be 16 _extAntennaReference_height

-- | AntennaDescriptor.
--
-- Antenna description information.
data AntennaDescriptor = AntennaDescriptor
  { _antennaDescriptor_num         :: Word16
    -- ^ Message number.
  , _antennaDescriptor_station     :: Word16
    -- ^ Reference station id.
  , _antennaDescriptor_n           :: Word8
    -- ^ Number of antenna descriptors.
  , _antennaDescriptor_descriptors :: [Word8]
    -- ^ Antenna descriptors.
  , _antennaDescriptor_setup       :: Word8
    -- ^ Antenna setup id.
  } deriving ( Show, Read, Eq )

$(makeLenses ''AntennaDescriptor)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_antennaDescriptor_" . stripPrefix "_antennaDescriptor_"} ''AntennaDescriptor)

instance BinaryBit AntennaDescriptor where
  getBits _n = do
    _antennaDescriptor_num         <- B.getWord16be 12
    _antennaDescriptor_station     <- B.getWord16be 12
    _antennaDescriptor_n           <- B.getWord8 8
    _antennaDescriptor_descriptors <- replicateM (fromIntegral _antennaDescriptor_n) $ B.getWord8 8
    _antennaDescriptor_setup       <- B.getWord8 8
    return AntennaDescriptor {..}

  putBits _n AntennaDescriptor {..} = do
    B.putWord16be 12 _antennaDescriptor_num
    B.putWord16be 12 _antennaDescriptor_station
    B.putWord8 8     _antennaDescriptor_n
    forM_ _antennaDescriptor_descriptors $ B.putWord8 8
    B.putWord8 8 _antennaDescriptor_setup

-- | ExtAntennaDescriptor.
--
-- Extended antenna description information.
data ExtAntennaDescriptor = ExtAntennaDescriptor
  { _extAntennaDescriptor_n             :: Word8
    -- ^ Number of antenna serial numbers.
  , _extAntennaDescriptor_serialNumbers :: [Word8]
    -- ^ Antenna serial numbers.
  } deriving ( Show, Read, Eq )

$(makeLenses ''ExtAntennaDescriptor)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_extAntennaDescriptor_" . stripPrefix "_extAntennaDescriptor_"} ''ExtAntennaDescriptor)

instance BinaryBit ExtAntennaDescriptor where
  getBits _n = do
    _extAntennaDescriptor_n             <- B.getWord8 8
    _extAntennaDescriptor_serialNumbers <- replicateM (fromIntegral _extAntennaDescriptor_n) $ B.getWord8 8
    return ExtAntennaDescriptor {..}

  putBits _n ExtAntennaDescriptor {..} = do
    B.putWord8 8 _extAntennaDescriptor_n
    forM_ _extAntennaDescriptor_serialNumbers $ B.putWord8 8

-- | ReceiverDescriptor.
--
-- Receiver description information.
data ReceiverDescriptor = ReceiverDescriptor
  { _receiverDescriptor_n                :: Word8
    -- ^ Number of receiver descriptors.
  , _receiverDescriptor_descriptors      :: [Word8]
    -- ^ Receiver descriptors.
  , _receiverDescriptor_m                :: Word8
    -- ^ Number of firmware versions.
  , _receiverDescriptor_firmwareVersions :: [Word8]
    -- ^ Firmware versions.
  , _receiverDescriptor_l                :: Word8
    -- ^ Number of serial numbers.
  , _receiverDescriptor_serialNumbers    :: [Word8]
    -- ^ Serial numbers.
  } deriving ( Show, Read, Eq )

$(makeLenses ''ReceiverDescriptor)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_receiverDescriptor_" . stripPrefix "_receiverDescriptor_"} ''ReceiverDescriptor)

instance BinaryBit ReceiverDescriptor where
  getBits _n = do
    _receiverDescriptor_n                <- B.getWord8 8
    _receiverDescriptor_descriptors      <- replicateM (fromIntegral _receiverDescriptor_n) $ B.getWord8 8
    _receiverDescriptor_m                <- B.getWord8 8
    _receiverDescriptor_firmwareVersions <- replicateM (fromIntegral _receiverDescriptor_m) $ B.getWord8 8
    _receiverDescriptor_l                <- B.getWord8 8
    _receiverDescriptor_serialNumbers    <- replicateM (fromIntegral _receiverDescriptor_l) $ B.getWord8 8
    return ReceiverDescriptor {..}

  putBits _n ReceiverDescriptor {..} = do
    B.putWord8 8     _receiverDescriptor_n
    forM_ _receiverDescriptor_descriptors $ B.putWord8 8
    B.putWord8 8     _receiverDescriptor_m
    forM_ _receiverDescriptor_firmwareVersions $ B.putWord8 8
    B.putWord8 8     _receiverDescriptor_l
    forM_ _receiverDescriptor_serialNumbers $ B.putWord8 8

msg1005 :: Word16
msg1005 = 1005

-- | Msg1005.
--
-- RTCMv3 message 1005.
newtype Msg1005 = Msg1005
  { _msg1005_reference :: AntennaReference
    -- ^ Antenna reference.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1005)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1005_" . stripPrefix "_msg1005_"} ''Msg1005)

instance Binary Msg1005 where
  get = B.runBitGet $ do
    _msg1005_reference <- getBits 0
    return Msg1005 {..}

  put Msg1005 {..} = B.runBitPut $
    putBits 0 _msg1005_reference

$(deriveRTCM3 ''Msg1005)

msg1006 :: Word16
msg1006 = 1006

-- | Msg1006.
--
-- RTCMv3 message 1006.
data Msg1006 = Msg1006
  { _msg1006_reference    :: AntennaReference
    -- ^ Antenna reference.
  , _msg1006_extReference :: ExtAntennaReference
    -- ^ Antenna extended reference.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1006)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1006_" . stripPrefix "_msg1006_"} ''Msg1006)

instance Binary Msg1006 where
  get = B.runBitGet $ do
    _msg1006_reference    <- getBits 0
    _msg1006_extReference <- getBits 0
    return Msg1006 {..}

  put Msg1006 {..} = B.runBitPut $ do
    putBits 0 _msg1006_reference
    putBits 0 _msg1006_extReference

$(deriveRTCM3 ''Msg1006)

msg1007 :: Word16
msg1007 = 1007

-- | Msg1007.
--
-- RTCMv3 message 1007.
newtype Msg1007 = Msg1007
  { _msg1007_descriptor :: AntennaDescriptor
    -- ^ Antenna descriptor.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1007)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1007_" . stripPrefix "_msg1007_"} ''Msg1007)

instance Binary Msg1007 where
  get = B.runBitGet $ do
    _msg1007_descriptor <- getBits 0
    return Msg1007 {..}

  put Msg1007 {..} = B.runBitPut $
    putBits 0 _msg1007_descriptor

$(deriveRTCM3 ''Msg1007)

msg1008 :: Word16
msg1008 = 1008

-- | Msg1008.
--
-- RTCMv3 message 1008.
data Msg1008 = Msg1008
  { _msg1008_descriptor    :: AntennaDescriptor
    -- ^ Antenna descriptor.
  , _msg1008_extDescriptor :: ExtAntennaDescriptor
    -- ^ Antenna extended descriptor.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1008)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1008_" . stripPrefix "_msg1008_"} ''Msg1008)

instance Binary Msg1008 where
  get = B.runBitGet $ do
    _msg1008_descriptor    <- getBits 0
    _msg1008_extDescriptor <- getBits 0
    return Msg1008 {..}

  put Msg1008 {..} = B.runBitPut $ do
    putBits 0 _msg1008_descriptor
    putBits 0 _msg1008_extDescriptor

$(deriveRTCM3 ''Msg1008)

msg1033 :: Word16
msg1033 = 1033

-- | Msg1033.
--
-- RTCMv3 message 1033.
data Msg1033 = Msg1033
  { _msg1033_antennaDescriptor    :: AntennaDescriptor
    -- ^ Antenna descriptor.
  , _msg1033_antennaExtDescriptor :: ExtAntennaDescriptor
    -- ^ Antenna extended descriptor.
  , _msg1033_receiverDescriptor   :: ReceiverDescriptor
    -- ^ Antenna descriptor.
  } deriving ( Show, Read, Eq)

$(makeLenses ''Msg1033)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1033_" . stripPrefix "_msg1033_"} ''Msg1033)

instance Binary Msg1033 where
  get = B.runBitGet $ do
    _msg1033_antennaDescriptor    <- getBits 0
    _msg1033_antennaExtDescriptor <- getBits 0
    _msg1033_receiverDescriptor   <- getBits 0
    return Msg1033 {..}

  put Msg1033 {..} = B.runBitPut $ do
    putBits 0 _msg1033_antennaDescriptor
    putBits 0 _msg1033_antennaExtDescriptor
    putBits 0 _msg1033_receiverDescriptor

$(deriveRTCM3 ''Msg1033)
