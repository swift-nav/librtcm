{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module:      Data.RTCM3.Ephemeris
-- Copyright:   Copyright (C) 2017 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- RTCMv3 Ephemeris messages

module Data.RTCM3.Ephemeris where

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

-- | EphemerisHeader.
--
-- GPS and Glonass Ephemeris header.
data EphemerisHeader = EphemerisHeader
  { _ephemerisHeader_num :: Word16
    -- ^ Message number.
  , _ephemerisHeader_sat :: Word8
    -- ^ GPS/Glonass satellite ID.
  } deriving ( Show, Read, Eq )

$(makeLenses ''EphemerisHeader)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_ephemerisHeader_" . stripPrefix "_ephemerisHeader_"} ''EphemerisHeader)

instance BinaryBit EphemerisHeader where
  getBits _n = do
    _ephemerisHeader_num <- B.getWord16be 12
    _ephemerisHeader_sat <- B.getWord8    6
    return EphemerisHeader {..}

  putBits _n EphemerisHeader {..} = do
    B.putWord16be 12 _ephemerisHeader_num
    B.putWord8    6  _ephemerisHeader_sat

data GpsEphemeris = GpsEphemeris
  { _gpsEphemeris_wn          :: Word16
    -- ^ GPS week number, mod 1024 (0-1023).
  , _gpsEphemeris_svAccuracy  :: Word8
    -- ^ GPS SV accuracy
  , _gpsEphemeris_codeOnL2    :: Word8
    -- ^ GPS code on L2: 00 reserved, 01 P code on, 10 C/A code on, 11 L2C on.
  , _gpsEphemeris_idot        :: Int16
    -- ^ GPS IDOT (rate of inclination angle). Unit: semi-circles/sec. Scale factor: 2^(-43)
  , _gpsEphemeris_iode        :: Word8
    -- ^ GPS IODE.
  , _gpsEphemeris_toc         :: Word16
    -- ^ GPS t_oc. Unit: Seconds. Scale factor: 2^4
  , _gpsEphemeris_af2         :: Int8
    -- ^ GPS a_f2. Unit: sec/sec^2. Scale factor: 2^(-55)
  , _gpsEphemeris_af1         :: Int16
    -- ^ GPS a_f1. Unit: sec/sec. Scale factor: 2^(-43)
  , _gpsEphemeris_af0         :: Int32
    -- ^ GPS a_f0. Unit: seconds. Scale factor: 2^(-31)
  , _gpsEphemeris_iodc        :: Word16
    -- ^ GPS iodc.
  , _gpsEphemeris_c_rs        :: Int16
    -- ^ GPS C_rs. Unit: Meters. Scale factor: 2^(-5)
  , _gpsEphemeris_dn          :: Int16
    -- ^ GPS delta n. Unit: semi-circles/sec. Scale factor: 2^(-43)
  , _gpsEphemeris_m0          :: Int32
    -- ^ GPS M_0. Unit: semi-circles. Scale factor: 2^(-31)
  , _gpsEphemeris_c_uc        :: Int16
    -- ^ GPS C_uc. Unit: radians. Scale factor: 2^(-29)
  , _gpsEphemeris_ecc         :: Word32
    -- ^ GPS Eccentricity (e). Unit: Dimensionless. Scale factor: 2^(-33)
  , _gpsEphemeris_c_us        :: Int16
    -- ^ GPS C_us. Unit: radians. Scale factor: 2^(-29)
  , _gpsEphemeris_sqrta       :: Word32
    -- ^ GPS A^(1/2). Unit: meters^(1/2). Scale factor: 2^(-19)
  , _gpsEphemeris_toe         :: Word16
    -- ^ GPS t_oe. Unit: seconds. Scale factor: 2^4
  , _gpsEphemeris_c_ic        :: Int16
    -- ^ GPS C_ic. Unit: radians. Scale factor: 2^(-29)
  , _gpsEphemeris_omega0      :: Int32
    -- ^ GPS Omega_0. Unit: semi-circles. Scale factor: 2^(-31)
  , _gpsEphemeris_c_is        :: Int16
    -- ^ GPS C_is. Unit: radians. Scale factor: 2^(-29)
  , _gpsEphemeris_i0          :: Int32
    -- ^ GPS i_0 (inclination angle at reference time; inc). Unit: semi-circles. Scale factor: 2^(-31)
  , _gpsEphemeris_c_rc        :: Int16
    -- ^ GPS C_rc. Unit: meters. Scale factor: 2^(-5)
  , _gpsEphemeris_w           :: Int32
    -- ^ GPS Argument of Perigee (omega, w). Unit: semi-circles. Scale factor: 2^(-31)
  , _gpsEphemeris_omegadot    :: Int32
    -- ^ GPS Omegadot - rate or right ascension. Unit: semi-circles/sec. Scale factor: 2^(-43)
  , _gpsEphemeris_tgd         :: Int8
    -- ^ GPS t_GD. Unit: seconds. Scale factor: 2^(-31)
  , _gpsEphemeris_svHealth    :: Word8
    -- ^ GPS SV health
  , _gpsEphemeris_l2pFlag     :: Bool
    -- ^ GPS L2P data flag
  , _gpsEphemeris_fitInterval :: Bool
    -- ^ GPS fit interval
  } deriving (Show, Read, Eq)

instance BinaryBit GpsEphemeris where
  getBits _n = do
    _gpsEphemeris_wn          <- B.getWord16be 10
    _gpsEphemeris_svAccuracy  <- B.getWord8    4
    _gpsEphemeris_codeOnL2    <- B.getWord8    2
    _gpsEphemeris_idot        <- getInt16be    14
    _gpsEphemeris_iode        <- B.getWord8    8
    _gpsEphemeris_toc         <- B.getWord16be 16
    _gpsEphemeris_af2         <- getInt8       8
    _gpsEphemeris_af1         <- getInt16be    16
    _gpsEphemeris_af0         <- getInt32be    22
    _gpsEphemeris_iodc        <- B.getWord16be 10
    _gpsEphemeris_c_rs        <- getInt16be    16
    _gpsEphemeris_dn          <- getInt16be    16
    _gpsEphemeris_m0          <- getInt32be    32
    _gpsEphemeris_c_uc        <- getInt16be    16
    _gpsEphemeris_ecc         <- B.getWord32be 32
    _gpsEphemeris_c_us        <- getInt16be    16
    _gpsEphemeris_sqrta       <- B.getWord32be 32
    _gpsEphemeris_toe         <- B.getWord16be 16
    _gpsEphemeris_c_ic        <- getInt16be    16
    _gpsEphemeris_omega0      <- getInt32be    32
    _gpsEphemeris_c_is         <- getInt16be    16
    _gpsEphemeris_i0          <- getInt32be    32
    _gpsEphemeris_c_rc        <- getInt16be    16
    _gpsEphemeris_w           <- getInt32be    32
    _gpsEphemeris_omegadot    <- getInt32be    24
    _gpsEphemeris_tgd         <- getInt8       8
    _gpsEphemeris_svHealth    <- B.getWord8    6
    _gpsEphemeris_l2pFlag     <- getBits 1
    _gpsEphemeris_fitInterval <- getBits 1
    return GpsEphemeris {..}

  putBits _n GpsEphemeris {..} = do
    B.putWord16be 10 _gpsEphemeris_wn
    B.putWord8    4  _gpsEphemeris_svAccuracy
    B.putWord8    2  _gpsEphemeris_codeOnL2
    putInt16be    14 _gpsEphemeris_idot
    B.putWord8    8  _gpsEphemeris_iode
    B.putWord16be 16 _gpsEphemeris_toc
    putInt8       8  _gpsEphemeris_af2
    putInt16be    16 _gpsEphemeris_af1
    putInt32be    22 _gpsEphemeris_af0
    B.putWord16be 10 _gpsEphemeris_iodc
    putInt16be    16 _gpsEphemeris_c_rs
    putInt16be    16 _gpsEphemeris_dn
    putInt32be    32 _gpsEphemeris_m0
    putInt16be    16 _gpsEphemeris_c_uc
    B.putWord32be 32 _gpsEphemeris_ecc
    putInt16be    16 _gpsEphemeris_c_us
    B.putWord32be 32 _gpsEphemeris_sqrta
    B.putWord16be 16 _gpsEphemeris_toe
    putInt16be    16 _gpsEphemeris_c_ic
    putInt32be    32 _gpsEphemeris_omega0
    putInt16be    16 _gpsEphemeris_c_is
    putInt32be    32 _gpsEphemeris_i0
    putInt16be    16 _gpsEphemeris_c_rc
    putInt32be    32 _gpsEphemeris_w
    putInt32be    24 _gpsEphemeris_omegadot
    putInt8       8  _gpsEphemeris_tgd
    B.putWord8    6  _gpsEphemeris_svHealth
    B.putBool        _gpsEphemeris_l2pFlag
    B.putBool        _gpsEphemeris_fitInterval

$(makeLenses ''GpsEphemeris)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_gpsEphemeris_" . stripPrefix "_gpsEphemeris_"} ''GpsEphemeris)

msg1019 :: Word16
msg1019 = 1019

-- | Msg 1019.
--
-- RTCMv3 message 1019.
--
-- See RTCM spec and GPS SPS Signal Specification, 2.4.3,
-- for more information about these fields.
data Msg1019 = Msg1019
  { _msg1019_header    :: EphemerisHeader
    -- ^ GPS ephemeris header.
  , _msg1019_ephemeris :: GpsEphemeris
    -- ^ GPS ephemeris body.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1019)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1019_" . stripPrefix "_msg1019_"} ''Msg1019)

instance Binary Msg1019 where
  get = B.runBitGet $ do
    _msg1019_header    <- getBits 0
    _msg1019_ephemeris <- getBits 0
    return Msg1019 {..}

  put Msg1019 {..} = B.runBitPut $ do
    putBits 0 _msg1019_header
    putBits 0 _msg1019_ephemeris

$(deriveRTCM3 ''Msg1019)
