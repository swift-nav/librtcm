{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
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

instance Arbitrary EphemerisHeader where
  arbitrary = do
    _ephemerisHeader_num <- arbitraryWord 12
    _ephemerisHeader_sat <- arbitraryWord 6
    return EphemerisHeader {..}

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

tests :: TestTree
tests =
  testGroup "Observations tests"
    [ testMsg1019
    ]
