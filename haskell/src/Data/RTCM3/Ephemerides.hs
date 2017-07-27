{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module:      Data.RTCM3.Ephemerides
-- Copyright:   Copyright (C) 2017 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- RTCMv3 Ephemerides messages

module Data.RTCM3.Ephemerides where

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

-- | GpsEphemerisHeader.
--
-- GPS and Glonass Ephemeris header.
data GpsEphemerisHeader = GpsEphemerisHeader
  { _gpsEphemerisHeader_num :: Word16
    -- ^ Message number.
  , _gpsEphemerisHeader_sat :: Word8
    -- ^ GPS/Glonass satellite ID.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsEphemerisHeader)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_gpsEphemerisHeader_" . stripPrefix "_gpsEphemerisHeader_"} ''GpsEphemerisHeader)

instance BinaryBit GpsEphemerisHeader where
  getBits _n = do
    _gpsEphemerisHeader_num <- B.getWord16be 12
    _gpsEphemerisHeader_sat <- B.getWord8    6
    return GpsEphemerisHeader {..}

  putBits _n GpsEphemerisHeader {..} = do
    B.putWord16be 12 _gpsEphemerisHeader_num
    B.putWord8    6  _gpsEphemerisHeader_sat

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
    _gpsEphemeris_l2pFlag     <- B.getBool
    _gpsEphemeris_fitInterval <- B.getBool
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

data GlonassEphemerisHeader = GlonassEphemerisHeader
  { _glonassEphemerisHeader_num     :: Word16
    -- ^ RTCM message number
  , _glonassEphemerisHeader_sat     :: Word8
    -- ^ GLONASS Satellite ID (slot number)
  , _glonassEphemerisHeader_channel :: Word8
    -- ^ Satellite frequency channel number
  } deriving ( Show, Read, Eq )

$(makeLenses ''GlonassEphemerisHeader)
$(deriveJSON
   defaultOptions
   { fieldLabelModifier =
       fromMaybe "_glonassEphemerisHeader_" .
       stripPrefix "_glonassEphemerisHeader_" }
   ''GlonassEphemerisHeader)

instance BinaryBit GlonassEphemerisHeader where
  getBits _n = do
    _glonassEphemerisHeader_num <- B.getWord16be  12
    _glonassEphemerisHeader_sat <- B.getWord8     6
    _glonassEphemerisHeader_channel <- B.getWord8 5
    return GlonassEphemerisHeader {..}

  putBits _n GlonassEphemerisHeader {..} = do
    B.putWord16be 12 _glonassEphemerisHeader_num
    B.putWord8    6  _glonassEphemerisHeader_sat
    B.putWord8    5  _glonassEphemerisHeader_channel

data GlonassEphemeris = GlonassEphemeris
  { _glonassEphemeris_almanacHealth      :: Bool
  -- ^ @C_n@ word
  , _glonassEphemeris_healthAvailability :: Bool
  -- ^ Is GLONASS almanac health available?
  , _glonassEphemeris_p1                 :: Word8
  -- ^ GLONASS P1 word
  , _glonassEphemeris_tk                 :: Word16
  -- ^ Time referenced to the beginning of GLONASS subframe within the
  -- current day.  See DF107 on page 55 of the RTCM3.1 document for
  -- more information.
  , _glonassEphemeris_bn_msb             :: Bool
  -- ^ MSB of B_n word: the ephemeris health flag
  , _glonassEphemeris_p2                 :: Bool
  -- ^ GLONASS P2 word
  , _glonassEphemeris_tb                 :: Word8
  -- ^ Time to which GLONASS navigation data are referenced.  Unit: 15 minutes
  , _glonassEphemeris_xndot              :: Int32
  -- ^ GLONASS @x_n(t_b)@, first time-derivative, given in PZ-90 datum.
  -- Unit: km/s Scale factor: 2^(-20)
  , _glonassEphemeris_xn                 :: Int32
  -- ^ GLONASS @x_n(t_b)@, given in PZ-90 datum.
  -- Unit: km Scale factor: 2^(-11)
  , _glonassEphemeris_xndotdot           :: Int8
  -- ^ GLONASS @x_n(t_b)@, second time-derivative, given in PZ-90 datum.
  -- Unit: km/s^2 Scale factor: 2^(-30)
  , _glonassEphemeris_yndot              :: Int32
  -- ^ GLONASS @y_n(t_b)@, first time-derivative, given in PZ-90 datum.
  -- Unit: km/s Scale factor: 2^(-20)
  , _glonassEphemeris_yn                 :: Int32
  -- ^ GLONASS @y_n(t_b)@, given in PZ-90 datum.
  -- Unit: km Scale factor: 2^(-11)
  , _glonassEphemeris_yndotdot           :: Int8
  -- ^ GLONASS @y_n(t_b)@, second time-derivative, given in PZ-90 datum.
  -- Unit: km/s^2 Scale factor: 2^(-30)
  , _glonassEphemeris_zndot              :: Int32
  -- ^ GLONASS @z_n(t_b)@, first time-derivative, given in PZ-90 datum.
  -- Unit: km/s Scale factor: 2^(-20)
  , _glonassEphemeris_zn                 :: Int32
  -- ^ GLONASS @z_n(t_b)@, given in PZ-90 datum.
  -- Unit: km Scale factor: 2^(-11)
  , _glonassEphemeris_zndotdot           :: Int8
  -- ^ GLONASS @z_n(t_b)@, second time-derivative, given in PZ-90 datum.
  -- Unit: km/s^2 Scale factor: 2^(-30)
  , _glonassEphemeris_p3                 :: Bool
  -- ^ GLONASS P3 word
  , _glonassEphemeris_gammaN             :: Int16
  -- ^ Relative deviation of predicted carrier from nominal value.
  -- Scale factor: 2^(-40)
  , _glonassEphemeris_mp                 :: Word8
  -- ^ GLONASS-M P-word
  , _glonassEphemeris_mi3                :: Bool
  -- ^ GLONASS-M I_n word extracted from third string of the subframe
  -- [this is the extent of the explanation given in the RTCM
  -- document]
  , _glonassEphemeris_tauN               :: Int32
  -- ^ GLONASS @tau_n(t_b)@, correction to the satellite time relative
  -- to GLONASS system time.
  -- Units: seconds  Scale factor: 2^(-30)
  , _glonassEphemeris_mdeltatau          :: Int8
  -- ^ GLONASS time difference between navigation RF signal
  -- transmitted in L2 sub-band and navigation RF signal transmitted
  -- in L1 sub-band
  -- Units: seconds  Scale factor: 2^(-30)
  , _glonassEphemeris_en                 :: Word8
  -- ^ GLONASS E_n, age of GLONASS navigation data
  -- Units: days  Scale factor: 1
  , _glonassEphemeris_mp4                :: Bool
  -- ^ GLONASS-M P4 word
  , _glonassEphemeris_mft                :: Word8
  -- ^ GLONASS-M predicted satellite user range accuracy at time @t_b@
  , _glonassEphemeris_mnt                :: Word16
  -- ^ GLONASS_M calendar number of day within four-year interval
  -- starting from the first of January in a leap year
  -- Units: days  Scale factor: 1
  , _glonassEphemeris_mM                 :: Word8
  -- ^ Type of GLONASS satellite:
    --     [@01@] -> GLONASS-M
    --     [@00@] -> non-GLONASS-M
  , _glonassEphemeris_additional         :: Bool
  -- ^ Are the remaining fields ("additional data") valid?
  , _glonassEphemeris_nA                 :: Word16
  -- ^ GLONASS calendar number of day within the four-year period to
  -- which @tau_c@ is referenced.
  -- Units: days  Scale factor: 1
  , _glonassEphemeris_tauC               :: Int32
  -- ^ GLONASS @tau_c@: difference between GLONASS system time and
  -- UTC(SU), referenced to '_glonassEphemeris_nA'.
  -- Units: seconds  Scale factor: 2^(-31)
  , _glonassEphemeris_mn4                :: Word8
  -- ^ GLONASS-M @N_4@: four-year interval number starting from 1996
  -- Units: 4 years
  , _glonassEphemeris_mTauGps            :: Int32
  -- ^ GLONASS-M @tau_gps@: correction to GPS system time relative to
  -- GLONASS system time.
  -- Units: seconds  Scale factor: 2^(-30)
  , _glonassEphemeris_mln5               :: Bool
  -- ^ GLONASS-M @I_n@ word extracted from fifth string of the
  -- subframe [this is the extent of the explanation given in the RTCM
  -- document]
  , _glonassEphemeris_reserved           :: Word8
  -- ^ Reserved field.
  } deriving ( Show, Read, Eq )

$(makeLenses ''GlonassEphemeris)
$(deriveJSON defaultOptions
  { fieldLabelModifier = fromMaybe "_glonassEphemeris_" .
                         stripPrefix "_glonassEphemeris_"
  } ''GlonassEphemeris)

instance BinaryBit GlonassEphemeris where
  getBits _n = do
    _glonassEphemeris_almanacHealth      <- B.getBool
    _glonassEphemeris_healthAvailability <- B.getBool
    _glonassEphemeris_p1                 <- B.getWord8 2
    _glonassEphemeris_tk                 <- B.getWord16be 12
    _glonassEphemeris_bn_msb             <- B.getBool
    _glonassEphemeris_p2                 <- B.getBool
    _glonassEphemeris_tb                 <- B.getWord8 7
    _glonassEphemeris_xndot              <- getInt32be 24
    _glonassEphemeris_xn                 <- getInt32be 27
    _glonassEphemeris_xndotdot           <- getInt8 5
    _glonassEphemeris_yndot              <- getInt32be 24
    _glonassEphemeris_yn                 <- getInt32be 27
    _glonassEphemeris_yndotdot           <- getInt8 5
    _glonassEphemeris_zndot              <- getInt32be 24
    _glonassEphemeris_zn                 <- getInt32be 27
    _glonassEphemeris_zndotdot           <- getInt8 5
    _glonassEphemeris_p3                 <- B.getBool
    _glonassEphemeris_gammaN             <- getInt16be 11
    _glonassEphemeris_mp                 <- B.getWord8 2
    _glonassEphemeris_mi3                <- B.getBool
    _glonassEphemeris_tauN               <- getInt32be 22
    _glonassEphemeris_mdeltatau          <- getInt8 5
    _glonassEphemeris_en                 <- B.getWord8 5
    _glonassEphemeris_mp4                <- B.getBool
    _glonassEphemeris_mft                <- B.getWord8 4
    _glonassEphemeris_mnt                <- B.getWord16be 11
    _glonassEphemeris_mM                 <- B.getWord8 2
    _glonassEphemeris_additional         <- B.getBool
    _glonassEphemeris_nA                 <- B.getWord16be 11
    _glonassEphemeris_tauC               <- getInt32be 32
    _glonassEphemeris_mn4                <- B.getWord8 5
    _glonassEphemeris_mTauGps            <- getInt32be 22
    _glonassEphemeris_mln5               <- B.getBool
    _glonassEphemeris_reserved           <- B.getWord8 7
    return GlonassEphemeris{..}

  putBits _n GlonassEphemeris{..} = do
    B.putBool        _glonassEphemeris_almanacHealth
    B.putBool        _glonassEphemeris_healthAvailability
    B.putWord8    2  _glonassEphemeris_p1
    B.putWord16be 12 _glonassEphemeris_tk
    B.putBool        _glonassEphemeris_bn_msb
    B.putBool        _glonassEphemeris_p2
    B.putWord8    7  _glonassEphemeris_tb
    putInt32be    24 _glonassEphemeris_xndot
    putInt32be    27 _glonassEphemeris_xn
    putInt8       5  _glonassEphemeris_xndotdot
    putInt32be    24 _glonassEphemeris_yndot
    putInt32be    27 _glonassEphemeris_yn
    putInt8       5  _glonassEphemeris_yndotdot
    putInt32be    24 _glonassEphemeris_zndot
    putInt32be    27 _glonassEphemeris_zn
    putInt8       5  _glonassEphemeris_zndotdot
    B.putBool        _glonassEphemeris_p3
    putInt16be    11 _glonassEphemeris_gammaN
    B.putWord8    2  _glonassEphemeris_mp
    B.putBool        _glonassEphemeris_mi3
    putInt32be    22 _glonassEphemeris_tauN
    putInt8       5  _glonassEphemeris_mdeltatau
    B.putWord8    5  _glonassEphemeris_en
    B.putBool        _glonassEphemeris_mp4
    B.putWord8    4  _glonassEphemeris_mft
    B.putWord16be 11 _glonassEphemeris_mnt
    B.putWord8    2  _glonassEphemeris_mM
    B.putBool        _glonassEphemeris_additional
    B.putWord16be 11 _glonassEphemeris_nA
    putInt32be    32 _glonassEphemeris_tauC
    B.putWord8    5  _glonassEphemeris_mn4
    putInt32be    22 _glonassEphemeris_mTauGps
    B.putBool        _glonassEphemeris_mln5
    B.putWord8    7  _glonassEphemeris_reserved

msg1019 :: Word16
msg1019 = 1019

-- | Msg 1019.
--
-- RTCMv3 message 1019.
--
-- See RTCM spec and GPS SPS Signal Specification, 2.4.3,
-- for more information about these fields.
data Msg1019 = Msg1019
  { _msg1019_header    :: GpsEphemerisHeader
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

msg1020 :: Word16
msg1020 = 1020

-- | Message 1020
--
-- RTCMv3 message 1020.
--
-- See RTCM spec and GLONASS signal specification for more information
-- about these fields.
data Msg1020 = Msg1020
  { _msg1020_header    :: GlonassEphemerisHeader
    -- ^ GLONASS ephemeris header.
  , _msg1020_ephemeris :: GlonassEphemeris
    -- ^ GLONASS ephemeris body.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1020)
$(deriveJSON defaultOptions
  { fieldLabelModifier = fromMaybe "_msg1020_" . stripPrefix "_msg1020_"
  } ''Msg1020)

instance Binary Msg1020 where
  get = B.runBitGet $ do
    _msg1020_header    <- getBits 0
    _msg1020_ephemeris <- getBits 0
    return Msg1020 {..}

  put Msg1020 {..} = B.runBitPut $ do
    putBits 0 _msg1020_header
    putBits 0 _msg1020_ephemeris

$(deriveRTCM3 ''Msg1020)
