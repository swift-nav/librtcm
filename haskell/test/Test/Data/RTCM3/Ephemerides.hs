{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS -fno-warn-orphans  #-}

-- |
-- Module:      Test.Data.RTCM3.Ephemerides
-- Copyright:   (c) 2017 Swift Navigation
-- License:     BSD3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
--
-- Test Ephemerides module for RTCM3.

module Test.Data.RTCM3.Ephemerides
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
    pure GpsEphemerisHeader {..}

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
    pure GpsEphemeris {..}

instance Arbitrary Msg1019 where
  arbitrary = do
    _msg1019_header    <- arbitrary
    _msg1019_ephemeris <- arbitrary
    pure Msg1019 {..}

instance Arbitrary GlonassEphemerisHeader where
  arbitrary = do
    _glonassEphemerisHeader_num <- arbitraryWord 12
    _glonassEphemerisHeader_sat <- arbitraryWord 6
    _glonassEphemerisHeader_channel <- arbitraryWord 5
    pure GlonassEphemerisHeader{..}

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
    _glonassEphemeris_reserved           <- arbitraryWord 7
    pure GlonassEphemeris{..}

instance Arbitrary Msg1020 where
  arbitrary = do
    _msg1020_header    <- arbitrary
    _msg1020_ephemeris <- arbitrary
    pure Msg1020 {..}

instance Arbitrary GalEphemerisHeader where
  arbitrary = do
    _galEphemerisHeader_num <- arbitraryWord 12
    _galEphemerisHeader_sat <- arbitraryWord 6
    pure GalEphemerisHeader {..}

instance Arbitrary GalEphemerisFnav where
  arbitrary = do
    _galEphemerisFnav_wn          <- arbitraryWord 12
    _galEphemerisFnav_iodnav      <- arbitraryWord 10
    _galEphemerisFnav_sisa        <- arbitraryWord 8
    _galEphemerisFnav_idot        <- arbitraryInt 14
    _galEphemerisFnav_toc         <- arbitraryWord 14
    _galEphemerisFnav_af2         <- arbitraryInt 6
    _galEphemerisFnav_af1         <- arbitraryInt 21
    _galEphemerisFnav_af0         <- arbitraryInt 31
    _galEphemerisFnav_c_rs        <- arbitraryInt 16
    _galEphemerisFnav_dn          <- arbitraryInt 16
    _galEphemerisFnav_m0          <- arbitraryInt 32
    _galEphemerisFnav_c_uc        <- arbitraryInt 16
    _galEphemerisFnav_ecc         <- arbitraryWord 32
    _galEphemerisFnav_c_us        <- arbitraryInt 16
    _galEphemerisFnav_sqrta       <- arbitraryWord 32
    _galEphemerisFnav_toe         <- arbitraryWord 14
    _galEphemerisFnav_c_ic        <- arbitraryInt 16
    _galEphemerisFnav_omega0      <- arbitraryInt 32
    _galEphemerisFnav_c_is        <- arbitraryInt 16
    _galEphemerisFnav_i0          <- arbitraryInt 32
    _galEphemerisFnav_c_rc        <- arbitraryInt 16
    _galEphemerisFnav_w           <- arbitraryInt 32
    _galEphemerisFnav_omegadot    <- arbitraryInt 24
    _galEphemerisFnav_bgdE5a      <- arbitraryInt 10
    _galEphemerisFnav_nav_health  <- arbitraryWord 2
    _galEphemerisFnav_validity    <- arbitraryWord 1
    _galEphemerisFnav_reserved    <- arbitraryWord 7
    pure GalEphemerisFnav {..}

instance Arbitrary GalEphemerisInav where
  arbitrary = do
    _galEphemerisInav_wn           <- arbitraryWord 12
    _galEphemerisInav_iodnav       <- arbitraryWord 10
    _galEphemerisInav_sisa         <- arbitraryWord 8
    _galEphemerisInav_idot         <- arbitraryInt 14
    _galEphemerisInav_toc          <- arbitraryWord 14
    _galEphemerisInav_af2          <- arbitraryInt 6
    _galEphemerisInav_af1          <- arbitraryInt 21
    _galEphemerisInav_af0          <- arbitraryInt 31
    _galEphemerisInav_c_rs         <- arbitraryInt 16
    _galEphemerisInav_dn           <- arbitraryInt 16
    _galEphemerisInav_m0           <- arbitraryInt 32
    _galEphemerisInav_c_uc         <- arbitraryInt 16
    _galEphemerisInav_ecc          <- arbitraryWord 32
    _galEphemerisInav_c_us         <- arbitraryInt 16
    _galEphemerisInav_sqrta        <- arbitraryWord 32
    _galEphemerisInav_toe          <- arbitraryWord 14
    _galEphemerisInav_c_ic         <- arbitraryInt 16
    _galEphemerisInav_omega0       <- arbitraryInt 32
    _galEphemerisInav_c_is         <- arbitraryInt 16
    _galEphemerisInav_i0           <- arbitraryInt 32
    _galEphemerisInav_c_rc         <- arbitraryInt 16
    _galEphemerisInav_w            <- arbitraryInt 32
    _galEphemerisInav_omegadot     <- arbitraryInt 24
    _galEphemerisInav_bgdE5a       <- arbitraryInt 10
    _galEphemerisInav_bgdE5b       <- arbitraryInt 10
    _galEphemerisInav_E5b_health   <- arbitraryWord 2
    _galEphemerisInav_E5b_validity <- arbitraryWord 1
    _galEphemerisInav_E1b_health   <- arbitraryWord 2
    _galEphemerisInav_E1b_validity <- arbitraryWord 1
    _galEphemerisInav_reserved     <- arbitraryWord 2
    pure GalEphemerisInav {..}

instance Arbitrary Msg1045 where
  arbitrary = do
    _msg1045_header    <- arbitrary
    _msg1045_ephemeris <- arbitrary
    pure Msg1045 {..}

instance Arbitrary Msg1046 where
  arbitrary = do
    _msg1046_header    <- arbitrary
    _msg1046_ephemeris <- arbitrary
    pure Msg1046 {..}

testMsg1019 :: TestTree
testMsg1019 =
  testProperty "Roundtrip Msg1019" $ \m ->
    decode (encode m) == (m :: Msg1019)

testMsg1020 :: TestTree
testMsg1020 =
  testProperty "Roundtrip Msg1020" $ \m ->
    decode (encode m) == (m :: Msg1020)

testMsg1045 :: TestTree
testMsg1045 =
  testProperty "Roundtrip Msg1045" $ \m ->
    decode (encode m) == (m :: Msg1045)

testMsg1046 :: TestTree
testMsg1046 =
  testProperty "Roundtrip Msg1046" $ \m ->
    decode (encode m) == (m :: Msg1046)

tests :: TestTree
tests =
  testGroup "Ephemeris tests"
    [ testMsg1019
    , testMsg1020
    , testMsg1045
    , testMsg1046
    ]
