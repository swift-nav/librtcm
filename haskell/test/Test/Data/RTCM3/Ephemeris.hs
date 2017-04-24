{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS -fno-warn-orphans #-}

-- |
-- Module:      Test.Data.RTCM3.Ephemeris
-- Copyright:   (c) 2017 Swift Navigation
-- License:     BSD3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
--
-- Test Ephemeris module for RTCM3.

module Test.Data.RTCM3.Ephemeris
  ( tests
  ) where

import BasicPrelude
import Data.Binary
import Data.RTCM3
import Test.Data.RTCM3.Test
import Test.Tasty
import Test.Tasty.QuickCheck

instance Arbitrary GpsEphemerisHeader where
  arbitrary = do
    _gpsEphemerisHeader_num <- arbitraryWord 12
    _gpsEphemerisHeader_sat <- arbitraryWord 6
    return GpsEphemerisHeader {..}

instance Arbitrary GpsEphemeris where
  arbitrary = do
    _gpsEphemeris_wn          <- arbitraryWord 10
    _gpsEphemeris_svAccuracy  <- arbitraryWord 4
    _gpsEphemeris_codeOnL2    <- arbitraryWord 2
    _gpsEphemeris_idot        <- arbitraryInt  14
    _gpsEphemeris_iode        <- arbitraryWord 8
    _gpsEphemeris_toc         <- arbitraryWord 16
    _gpsEphemeris_af2         <- arbitraryInt  8
    _gpsEphemeris_af1         <- arbitraryInt  16
    _gpsEphemeris_af0         <- arbitraryInt  22
    _gpsEphemeris_iodc        <- arbitraryWord 10
    _gpsEphemeris_c_rs        <- arbitraryInt  16
    _gpsEphemeris_dn          <- arbitraryInt  16
    _gpsEphemeris_m0          <- arbitraryInt  32
    _gpsEphemeris_c_uc        <- arbitraryInt  16
    _gpsEphemeris_ecc         <- arbitraryWord 32
    _gpsEphemeris_c_us        <- arbitraryInt  16
    _gpsEphemeris_sqrta       <- arbitraryWord 32
    _gpsEphemeris_toe         <- arbitraryWord 16
    _gpsEphemeris_c_ic        <- arbitraryInt  16
    _gpsEphemeris_omega0      <- arbitraryInt  32
    _gpsEphemeris_c_is        <- arbitraryInt  16
    _gpsEphemeris_i0          <- arbitraryInt  32
    _gpsEphemeris_c_rc        <- arbitraryInt  16
    _gpsEphemeris_w           <- arbitraryInt  32
    _gpsEphemeris_omegadot    <- arbitraryInt  24
    _gpsEphemeris_tgd         <- arbitraryInt  8
    _gpsEphemeris_svHealth    <- arbitraryWord 6
    _gpsEphemeris_l2pFlag     <- arbitrary
    _gpsEphemeris_fitInterval <- arbitrary
    return GpsEphemeris {..}

instance Arbitrary Msg1019 where
  arbitrary = do
    _msg1019_header    <- arbitrary
    _msg1019_ephemeris <- arbitrary
    return Msg1019 {..}

testMsg1019 :: TestTree
testMsg1019 =
  testProperty "Roundtrip Msg1019" $ \m ->
    (decode $ encode m) == (m :: Msg1019)

instance Arbitrary GlonassEphemerisHeader where
  arbitrary = do
    _glonassEphemerisHeader_num <- arbitraryWord 12
    _glonassEphemerisHeader_sat <- arbitraryWord 6
    _glonassEphemerisHeader_channel <- arbitraryWord 5
    return GlonassEphemerisHeader{..}

instance Arbitrary GlonassEphemeris where
  arbitrary = do
    _glonassEphemeris_almanacHealth      <- arbitrary
    _glonassEphemeris_healthAvailability <- arbitrary
    _glonassEphemeris_p1                 <- arbitraryWord 2
    _glonassEphemeris_tk                 <- arbitraryWord 12
    _glonassEphemeris_bn_msb             <- arbitrary
    _glonassEphemeris_p2                 <- arbitrary
    _glonassEphemeris_tb                 <- arbitraryWord 7
    _glonassEphemeris_xndot              <- arbitraryInt 24
    _glonassEphemeris_xn                 <- arbitraryInt 27
    _glonassEphemeris_xndotdot           <- arbitraryInt 5
    _glonassEphemeris_yndot              <- arbitraryInt 24
    _glonassEphemeris_yn                 <- arbitraryInt 27
    _glonassEphemeris_yndotdot           <- arbitraryInt 5
    _glonassEphemeris_zndot              <- arbitraryInt 24
    _glonassEphemeris_zn                 <- arbitraryInt 27
    _glonassEphemeris_zndotdot           <- arbitraryInt 5
    _glonassEphemeris_p3                 <- arbitrary
    _glonassEphemeris_gammaN             <- arbitraryInt 11
    _glonassEphemeris_mp                 <- arbitraryWord 2
    _glonassEphemeris_mi3                <- arbitrary
    _glonassEphemeris_tauN               <- arbitraryInt 22
    _glonassEphemeris_mdeltatau          <- arbitraryInt 5
    _glonassEphemeris_en                 <- arbitraryWord 5
    _glonassEphemeris_mp4                <- arbitrary
    _glonassEphemeris_mft                <- arbitraryWord 4
    _glonassEphemeris_mnt                <- arbitraryWord 11
    _glonassEphemeris_mM                 <- arbitraryWord 2
    _glonassEphemeris_additional         <- arbitrary
    _glonassEphemeris_nA                 <- arbitraryWord 11
    _glonassEphemeris_tauC               <- arbitraryInt 32
    _glonassEphemeris_mn4                <- arbitraryWord 5
    _glonassEphemeris_mTauGps            <- arbitraryInt 22
    _glonassEphemeris_mln5               <- arbitrary
    return GlonassEphemeris{..}

instance Arbitrary Msg1020 where
  arbitrary = do
    _msg1020_header <- arbitrary
    _msg1020_ephemeris <- arbitrary
    return Msg1020 {..}

testMsg1020 :: TestTree
testMsg1020 =
  testProperty "Roundtrip Msg1020" $ \m ->
    (decode $ encode m) == (m :: Msg1020)

tests :: TestTree
tests =
  testGroup "Ephemeris tests"
    [ testMsg1019
    , testMsg1020
    ]
