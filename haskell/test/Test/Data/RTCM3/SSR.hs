{-# OPTIONS -fno-warn-orphans #-}

-- |
-- Module:      Test.Data.RTCM3.SSR
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
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
    return GpsOrbitCorrectionHeader {..}

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
    return GpsOrbitCorrection {..}

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
    return GpsClockCorrectionHeader {..}

instance Arbitrary GpsClockCorrection where
  arbitrary = do
    _gpsClockCorrection_sat          <- arbitraryWord 6
    _gpsClockCorrection_deltaClockC0 <- arbitraryInt 22
    _gpsClockCorrection_deltaClockC1 <- arbitraryInt 21
    _gpsClockCorrection_deltaClockC2 <- arbitraryInt 27
    return GpsClockCorrection {..}

instance Arbitrary Msg1057 where
  arbitrary = do
    _msg1057_header      <- arbitrary
    _msg1057_corrections <- replicateM (fromIntegral $ _msg1057_header ^. gpsOrbitCorrectionHeader_n) $ arbitrary
    return Msg1057 {..}

instance Arbitrary Msg1058 where
  arbitrary = do
    _msg1058_header      <- arbitrary
    _msg1058_corrections <- replicateM (fromIntegral $ _msg1058_header ^. gpsClockCorrectionHeader_n) $ arbitrary
    return Msg1058 {..}

testMsg1057 :: TestTree
testMsg1057 =
  testProperty "Roundtrip Msg1057" $ \m ->
    (decode $ encode m) == (m :: Msg1057)

testMsg1058 :: TestTree
testMsg1058 =
  testProperty "Roundtrip Msg1058" $ \m ->
    (decode $ encode m) == (m :: Msg1058)

tests :: TestTree
tests =
  testGroup "SSR tests"
    [ testMsg1057
    , testMsg1058
    ]
