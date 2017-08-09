{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS -fno-warn-orphans  #-}

-- |
-- Module:      Test.Data.RTCM3.SSR
-- Copyright:   (c) 2015 Swift Navigation
-- License:     BSD3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
--
-- Test SSR module for RTCM3.

module Test.Data.RTCM3.SSR
  ( tests
  ) where

import BasicPrelude
import Control.Lens
import Data.Binary
import Data.RTCM3
import Test.Data.RTCM3.Test
import Test.Tasty
import Test.Tasty.QuickCheck

instance Arbitrary GpsOrbitCorrectionHeader where
  arbitrary = do
    _gpsOrbitCorrectionHeader_num            <- arbitraryWord 12
    _gpsOrbitCorrectionHeader_epochs         <- arbitraryWord 20
    _gpsOrbitCorrectionHeader_updateInterval <- arbitraryWord 4
    _gpsOrbitCorrectionHeader_multiple       <- arbitrary
    _gpsOrbitCorrectionHeader_datum          <- arbitrary
    _gpsOrbitCorrectionHeader_iod            <- arbitraryWord 4
    _gpsOrbitCorrectionHeader_provider       <- arbitraryWord 16
    _gpsOrbitCorrectionHeader_solution       <- arbitraryWord 4
    _gpsOrbitCorrectionHeader_n              <- arbitraryWord 6
    pure GpsOrbitCorrectionHeader {..}

instance Arbitrary GpsOrbitCorrection where
  arbitrary = do
    _gpsOrbitCorrection_sat                <- arbitraryWord 6
    _gpsOrbitCorrection_iode               <- arbitraryWord 8
    _gpsOrbitCorrection_deltaRadial        <- arbitraryInt 22
    _gpsOrbitCorrection_deltaAlongTrack    <- arbitraryInt 20
    _gpsOrbitCorrection_deltaCrossTrack    <- arbitraryInt 20
    _gpsOrbitCorrection_dotDeltaRadial     <- arbitraryInt 21
    _gpsOrbitCorrection_dotDeltaAlongTrack <- arbitraryInt 19
    _gpsOrbitCorrection_dotDeltaCrossTrack <- arbitraryInt 19
    pure GpsOrbitCorrection {..}

instance Arbitrary GpsClockCorrectionHeader where
  arbitrary = do
    _gpsClockCorrectionHeader_num            <- arbitraryWord 12
    _gpsClockCorrectionHeader_epochs         <- arbitraryWord 20
    _gpsClockCorrectionHeader_updateInterval <- arbitraryWord 4
    _gpsClockCorrectionHeader_multiple       <- arbitrary
    _gpsClockCorrectionHeader_iod            <- arbitraryWord 4
    _gpsClockCorrectionHeader_provider       <- arbitraryWord 16
    _gpsClockCorrectionHeader_solution       <- arbitraryWord 4
    _gpsClockCorrectionHeader_n              <- arbitraryWord 6
    pure GpsClockCorrectionHeader {..}

instance Arbitrary GpsClockCorrection where
  arbitrary = do
    _gpsClockCorrection_sat          <- arbitraryWord 6
    _gpsClockCorrection_deltaClockC0 <- arbitraryInt 22
    _gpsClockCorrection_deltaClockC1 <- arbitraryInt 21
    _gpsClockCorrection_deltaClockC2 <- arbitraryInt 27
    pure GpsClockCorrection {..}

instance Arbitrary GlonassOrbitCorrectionHeader where
  arbitrary = do
    _glonassOrbitCorrectionHeader_num            <- arbitraryWord 12
    _glonassOrbitCorrectionHeader_epochs         <- arbitraryWord 17
    _glonassOrbitCorrectionHeader_updateInterval <- arbitraryWord 4
    _glonassOrbitCorrectionHeader_multiple       <- arbitrary
    _glonassOrbitCorrectionHeader_datum          <- arbitrary
    _glonassOrbitCorrectionHeader_iod            <- arbitraryWord 4
    _glonassOrbitCorrectionHeader_provider       <- arbitraryWord 16
    _glonassOrbitCorrectionHeader_solution       <- arbitraryWord 4
    _glonassOrbitCorrectionHeader_n              <- arbitraryWord 6
    pure GlonassOrbitCorrectionHeader {..}

instance Arbitrary GlonassOrbitCorrection where
  arbitrary = do
    _glonassOrbitCorrection_sat                <- arbitraryWord 5
    _glonassOrbitCorrection_iode               <- arbitraryWord 8
    _glonassOrbitCorrection_deltaRadial        <- arbitraryInt 22
    _glonassOrbitCorrection_deltaAlongTrack    <- arbitraryInt 20
    _glonassOrbitCorrection_deltaCrossTrack    <- arbitraryInt 20
    _glonassOrbitCorrection_dotDeltaRadial     <- arbitraryInt 21
    _glonassOrbitCorrection_dotDeltaAlongTrack <- arbitraryInt 19
    _glonassOrbitCorrection_dotDeltaCrossTrack <- arbitraryInt 19
    pure GlonassOrbitCorrection {..}

instance Arbitrary GlonassClockCorrectionHeader where
  arbitrary = do
    _glonassClockCorrectionHeader_num            <- arbitraryWord 12
    _glonassClockCorrectionHeader_epochs         <- arbitraryWord 17
    _glonassClockCorrectionHeader_updateInterval <- arbitraryWord 4
    _glonassClockCorrectionHeader_multiple       <- arbitrary
    _glonassClockCorrectionHeader_iod            <- arbitraryWord 4
    _glonassClockCorrectionHeader_provider       <- arbitraryWord 16
    _glonassClockCorrectionHeader_solution       <- arbitraryWord 4
    _glonassClockCorrectionHeader_n              <- arbitraryWord 6
    pure GlonassClockCorrectionHeader {..}

instance Arbitrary GlonassClockCorrection where
  arbitrary = do
    _glonassClockCorrection_sat          <- arbitraryWord 5
    _glonassClockCorrection_deltaClockC0 <- arbitraryInt 22
    _glonassClockCorrection_deltaClockC1 <- arbitraryInt 21
    _glonassClockCorrection_deltaClockC2 <- arbitraryInt 27
    pure GlonassClockCorrection {..}

instance Arbitrary Msg1057 where
  arbitrary = do
    _msg1057_header      <- arbitrary
    _msg1057_corrections <- replicateM (fromIntegral $ _msg1057_header ^. gpsOrbitCorrectionHeader_n) $ arbitrary
    pure Msg1057 {..}

instance Arbitrary Msg1058 where
  arbitrary = do
    _msg1058_header      <- arbitrary
    _msg1058_corrections <- replicateM (fromIntegral $ _msg1058_header ^. gpsClockCorrectionHeader_n) $ arbitrary
    pure Msg1058 {..}

instance Arbitrary Msg1063 where
  arbitrary = do
    _msg1063_header      <- arbitrary
    _msg1063_corrections <- replicateM (fromIntegral $ _msg1063_header ^. glonassOrbitCorrectionHeader_n) $ arbitrary
    pure Msg1063 {..}

instance Arbitrary Msg1064 where
  arbitrary = do
    _msg1064_header      <- arbitrary
    _msg1064_corrections <- replicateM (fromIntegral $ _msg1064_header ^. glonassClockCorrectionHeader_n) $ arbitrary
    pure Msg1064 {..}

testMsg1057 :: TestTree
testMsg1057 =
  testProperty "Roundtrip Msg1057" $ \m ->
    (decode $ encode m) == (m :: Msg1057)

testMsg1058 :: TestTree
testMsg1058 =
  testProperty "Roundtrip Msg1058" $ \m ->
    (decode $ encode m) == (m :: Msg1058)

testMsg1063 :: TestTree
testMsg1063 =
  testProperty "Roundtrip Msg1063" $ \m ->
    (decode $ encode m) == (m :: Msg1063)

testMsg1064 :: TestTree
testMsg1064 =
  testProperty "Roundtrip Msg1064" $ \m ->
    (decode $ encode m) == (m :: Msg1064)

tests :: TestTree
tests =
  testGroup "SSR tests"
    [ testMsg1057
    , testMsg1058
    , testMsg1063
    , testMsg1064
    ]
