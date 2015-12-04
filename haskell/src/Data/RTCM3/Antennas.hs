-- |
-- Module:      Data.RTCM3.Antennas
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Mark Fine <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- RTCMv3 antenna messages for stationary references and descriptors.

module Data.RTCM3.Antennas where

import           BasicPrelude
import           Control.Lens
import           Data.Binary
import           Data.Binary.Bits
import qualified Data.Binary.Bits.Get as B
import qualified Data.Binary.Bits.Put as B
import           Data.RTCM3.Extras
import           Data.RTCM3.TH

-- | AntennaReference.
--
-- Stationary antenna reference point information.
data AntennaReference = AntennaReference
  { _antennaReference_num        :: Word16
    -- ^ Message number.
  , _antennaReference_station    :: Word16
    -- ^ Reference station id.
  , _antennaReference_gps        :: Bool
    -- ^ GPS indicator.
  , _antennaReference_glonass    :: Bool
    -- ^ GLONASS indicator.
  , _antennaReference_galileo    :: Bool
    -- ^ Galileo indicator.
  , _antennaReference_computed   :: Bool
    -- ^ Reference-station non-physical indicator.
  , _antennaReference_ecef_x     :: Int64
    -- ^ Antenna reference point ECEF-X.
  , _antennaReference_oscillator :: Bool
    -- ^ Single receiver oscillator indicator.
  , _antennaReference_ecef_y     :: Int64
    -- ^ Antenna reference point ECEF-Y.
  , _antennaReference_ecef_z     :: Int64
    -- ^ Antenna reference point ECEF-Z.
  } deriving ( Show, Read, Eq )

$(makeLenses ''AntennaReference)

instance BinaryBit AntennaReference where
  getBits _n = do
    _antennaReference_num        <- B.getWord16be 12
    _antennaReference_station    <- B.getWord16be 12
    _reserved                    <- B.getWord8 6
    _antennaReference_gps        <- B.getBool
    _antennaReference_glonass    <- B.getBool
    _antennaReference_galileo    <- B.getBool
    _antennaReference_computed   <- B.getBool
    _antennaReference_ecef_x     <- getInt64be 38
    _antennaReference_oscillator <- B.getBool
    _reserved                    <- B.getBool
    _antennaReference_ecef_y     <- getInt64be 38
    _reserved                    <- B.getWord8 2
    _antennaReference_ecef_z     <- getInt64be 38
    return AntennaReference {..}

  putBits _n AntennaReference {..} = do
    B.putWord16be 12 _antennaReference_num
    B.putWord16be 12 _antennaReference_station
    B.putWord8 6     0
    B.putBool        _antennaReference_gps
    B.putBool        _antennaReference_glonass
    B.putBool        _antennaReference_galileo
    B.putBool        _antennaReference_computed
    putInt64be 38    _antennaReference_ecef_x
    B.putBool        False
    putInt64be 38    _antennaReference_ecef_y
    B.putWord8 2     0
    putInt64be 38    _antennaReference_ecef_z

-- | ExtAntennaReference.
--
-- Extended stationary antenna reference point information.
data ExtAntennaReference = ExtAntennaReference
  { _extAntennaReference_height :: Word16
    -- ^ Antenna height.
  } deriving ( Show, Read, Eq )

$(makeLenses ''ExtAntennaReference)

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

instance BinaryBit ExtAntennaDescriptor where
  getBits _n = do
    _extAntennaDescriptor_n             <- B.getWord8 8
    _extAntennaDescriptor_serialNumbers <- replicateM (fromIntegral _extAntennaDescriptor_n) $ B.getWord8 8
    return ExtAntennaDescriptor {..}

  putBits _n ExtAntennaDescriptor {..} = do
    B.putWord8 8 _extAntennaDescriptor_n
    forM_ _extAntennaDescriptor_serialNumbers $ B.putWord8 8

msg1005 :: Word16
msg1005 = 1005

-- | Msg1005.
--
-- RTCMv3 message 1005.
data Msg1005 = Msg1005
  { _msg1005_reference :: AntennaReference
    -- ^ Antenna reference.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1005)

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
data Msg1007 = Msg1007
  { _msg1007_descriptor :: AntennaDescriptor
    -- ^ Antenna descriptor.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1007)

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

instance Binary Msg1008 where
  get = B.runBitGet $ do
    _msg1008_descriptor    <- getBits 0
    -- ^ Antenna descriptor.
    _msg1008_extDescriptor <- getBits 0
    -- ^ Antenna extended descriptor.
    return Msg1008 {..}

  put Msg1008 {..} = B.runBitPut $ do
    putBits 0 _msg1008_descriptor
    putBits 0 _msg1008_extDescriptor

$(deriveRTCM3 ''Msg1008)
