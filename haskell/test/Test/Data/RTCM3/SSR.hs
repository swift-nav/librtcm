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

instance Arbitrary GpsOrbitClockCorrectionHeader where
  arbitrary = do
    _gpsOrbitClockCorrectionHeader_num            <- arbitraryWord 12
    _gpsOrbitClockCorrectionHeader_epochs         <- arbitraryWord 20
    _gpsOrbitClockCorrectionHeader_updateInterval <- arbitraryWord 4
    _gpsOrbitClockCorrectionHeader_multiple       <- arbitrary
    _gpsOrbitClockCorrectionHeader_datum          <- arbitrary
    _gpsOrbitClockCorrectionHeader_iod            <- arbitraryWord 4
    _gpsOrbitClockCorrectionHeader_provider       <- arbitraryWord 16
    _gpsOrbitClockCorrectionHeader_solution       <- arbitraryWord 4
    _gpsOrbitClockCorrectionHeader_n              <- arbitraryWord 6
    pure GpsOrbitClockCorrectionHeader {..}

instance Arbitrary GpsOrbitClockCorrection where
  arbitrary = do
    _gpsOrbitClockCorrection_sat                <- arbitraryWord 6
    _gpsOrbitClockCorrection_iode               <- arbitraryWord 8
    _gpsOrbitClockCorrection_deltaRadial        <- arbitraryInt 22
    _gpsOrbitClockCorrection_deltaAlongTrack    <- arbitraryInt 20
    _gpsOrbitClockCorrection_deltaCrossTrack    <- arbitraryInt 20
    _gpsOrbitClockCorrection_dotDeltaRadial     <- arbitraryInt 21
    _gpsOrbitClockCorrection_dotDeltaAlongTrack <- arbitraryInt 19
    _gpsOrbitClockCorrection_dotDeltaCrossTrack <- arbitraryInt 19
    _gpsOrbitClockCorrection_deltaClockC0       <- arbitraryInt 22
    _gpsOrbitClockCorrection_deltaClockC1       <- arbitraryInt 21
    _gpsOrbitClockCorrection_deltaClockC2       <- arbitraryInt 27
    pure GpsOrbitClockCorrection {..}

instance Arbitrary GlonassOrbitClockCorrectionHeader where
  arbitrary = do
    _glonassOrbitClockCorrectionHeader_num            <- arbitraryWord 12
    _glonassOrbitClockCorrectionHeader_epochs         <- arbitraryWord 17
    _glonassOrbitClockCorrectionHeader_updateInterval <- arbitraryWord 4
    _glonassOrbitClockCorrectionHeader_multiple       <- arbitrary
    _glonassOrbitClockCorrectionHeader_datum          <- arbitrary
    _glonassOrbitClockCorrectionHeader_iod            <- arbitraryWord 4
    _glonassOrbitClockCorrectionHeader_provider       <- arbitraryWord 16
    _glonassOrbitClockCorrectionHeader_solution       <- arbitraryWord 4
    _glonassOrbitClockCorrectionHeader_n              <- arbitraryWord 6
    pure GlonassOrbitClockCorrectionHeader {..}

instance Arbitrary GlonassOrbitClockCorrection where
  arbitrary = do
    _glonassOrbitClockCorrection_sat                <- arbitraryWord 5
    _glonassOrbitClockCorrection_iode               <- arbitraryWord 8
    _glonassOrbitClockCorrection_deltaRadial        <- arbitraryInt 22
    _glonassOrbitClockCorrection_deltaAlongTrack    <- arbitraryInt 20
    _glonassOrbitClockCorrection_deltaCrossTrack    <- arbitraryInt 20
    _glonassOrbitClockCorrection_dotDeltaRadial     <- arbitraryInt 21
    _glonassOrbitClockCorrection_dotDeltaAlongTrack <- arbitraryInt 19
    _glonassOrbitClockCorrection_dotDeltaCrossTrack <- arbitraryInt 19
    _glonassOrbitClockCorrection_deltaClockC0       <- arbitraryInt 22
    _glonassOrbitClockCorrection_deltaClockC1       <- arbitraryInt 21
    _glonassOrbitClockCorrection_deltaClockC2       <- arbitraryInt 27
    pure GlonassOrbitClockCorrection {..}

instance Arbitrary GpsCodeBiasCorrectionHeader where
  arbitrary = do
    _gpsCodeBiasCorrectionHeader_num            <- arbitraryWord 12
    _gpsCodeBiasCorrectionHeader_epochs         <- arbitraryWord 20
    _gpsCodeBiasCorrectionHeader_updateInterval <- arbitraryWord 4
    _gpsCodeBiasCorrectionHeader_multiple       <- arbitrary
    _gpsCodeBiasCorrectionHeader_iod            <- arbitraryWord 4
    _gpsCodeBiasCorrectionHeader_provider       <- arbitraryWord 16
    _gpsCodeBiasCorrectionHeader_solution       <- arbitraryWord 4
    _gpsCodeBiasCorrectionHeader_n              <- arbitraryWord 6
    pure GpsCodeBiasCorrectionHeader {..}

instance Arbitrary GpsCodeBias where
  arbitrary = do
    _gpsCodeBias_signal   <- arbitraryWord 5
    _gpsCodeBias_codeBias <- arbitraryInt 14
    pure GpsCodeBias {..}

instance Arbitrary GpsCodeBiasCorrection where
  arbitrary = do
    _gpsCodeBiasCorrection_sat        <- arbitraryWord 6
    _gpsCodeBiasCorrection_n          <- arbitraryWord 5
    _gpsCodeBiasCorrection_codeBiases <- replicateM (fromIntegral _gpsCodeBiasCorrection_n) arbitrary
    pure GpsCodeBiasCorrection {..}

instance Arbitrary GlonassCodeBiasCorrectionHeader where
  arbitrary = do
    _glonassCodeBiasCorrectionHeader_num            <- arbitraryWord 12
    _glonassCodeBiasCorrectionHeader_epochs         <- arbitraryWord 17
    _glonassCodeBiasCorrectionHeader_updateInterval <- arbitraryWord 4
    _glonassCodeBiasCorrectionHeader_multiple       <- arbitrary
    _glonassCodeBiasCorrectionHeader_iod            <- arbitraryWord 4
    _glonassCodeBiasCorrectionHeader_provider       <- arbitraryWord 16
    _glonassCodeBiasCorrectionHeader_solution       <- arbitraryWord 4
    _glonassCodeBiasCorrectionHeader_n              <- arbitraryWord 6
    pure GlonassCodeBiasCorrectionHeader {..}

instance Arbitrary GlonassCodeBias where
  arbitrary = do
    _glonassCodeBias_signal   <- arbitraryWord 5
    _glonassCodeBias_codeBias <- arbitraryInt 14
    pure GlonassCodeBias {..}

instance Arbitrary GlonassCodeBiasCorrection where
  arbitrary = do
    _glonassCodeBiasCorrection_sat        <- arbitraryWord 5
    _glonassCodeBiasCorrection_n          <- arbitraryWord 5
    _glonassCodeBiasCorrection_codeBiases <- replicateM (fromIntegral _glonassCodeBiasCorrection_n) arbitrary
    pure GlonassCodeBiasCorrection {..}

instance Arbitrary GpsPhaseBiasCorrectionHeader where
  arbitrary = do
    _gpsPhaseBiasCorrectionHeader_num            <- arbitraryWord 12
    _gpsPhaseBiasCorrectionHeader_epochs         <- arbitraryWord 20
    _gpsPhaseBiasCorrectionHeader_updateInterval <- arbitraryWord 4
    _gpsPhaseBiasCorrectionHeader_multiple       <- arbitrary
    _gpsPhaseBiasCorrectionHeader_iod            <- arbitraryWord 4
    _gpsPhaseBiasCorrectionHeader_provider       <- arbitraryWord 16
    _gpsPhaseBiasCorrectionHeader_solution       <- arbitraryWord 4
    _gpsPhaseBiasCorrectionHeader_dispersive     <- arbitrary
    _gpsPhaseBiasCorrectionHeader_mw             <- arbitrary
    _gpsPhaseBiasCorrectionHeader_n              <- arbitraryWord 6
    pure GpsPhaseBiasCorrectionHeader {..}

instance Arbitrary GpsPhaseBias where
  arbitrary = do
    _gpsPhaseBias_signal               <- arbitraryWord 5
    _gpsPhaseBias_integer              <- arbitrary
    _gpsPhaseBias_wideLaneInteger      <- arbitraryWord 2
    _gpsPhaseBias_discontinuityCounter <- arbitraryWord 4
    _gpsPhaseBias_phaseBias            <- arbitraryInt 20
    pure GpsPhaseBias {..}

instance Arbitrary GpsPhaseBiasCorrection where
  arbitrary = do
    _gpsPhaseBiasCorrection_sat         <- arbitraryWord 6
    _gpsPhaseBiasCorrection_n           <- arbitraryWord 5
    _gpsPhaseBiasCorrection_yawAngle    <- arbitraryWord 9
    _gpsPhaseBiasCorrection_yawRate     <- arbitraryInt 8
    _gpsPhaseBiasCorrection_phaseBiases <- replicateM (fromIntegral _gpsPhaseBiasCorrection_n) arbitrary
    pure GpsPhaseBiasCorrection {..}

instance Arbitrary GlonassPhaseBiasCorrectionHeader where
  arbitrary = do
    _glonassPhaseBiasCorrectionHeader_num            <- arbitraryWord 12
    _glonassPhaseBiasCorrectionHeader_epochs         <- arbitraryWord 17
    _glonassPhaseBiasCorrectionHeader_updateInterval <- arbitraryWord 4
    _glonassPhaseBiasCorrectionHeader_multiple       <- arbitrary
    _glonassPhaseBiasCorrectionHeader_iod            <- arbitraryWord 4
    _glonassPhaseBiasCorrectionHeader_provider       <- arbitraryWord 16
    _glonassPhaseBiasCorrectionHeader_solution       <- arbitraryWord 4
    _glonassPhaseBiasCorrectionHeader_dispersive     <- arbitrary
    _glonassPhaseBiasCorrectionHeader_mw             <- arbitrary
    _glonassPhaseBiasCorrectionHeader_n              <- arbitraryWord 6
    pure GlonassPhaseBiasCorrectionHeader {..}

instance Arbitrary GlonassPhaseBias where
  arbitrary = do
    _glonassPhaseBias_signal               <- arbitraryWord 5
    _glonassPhaseBias_integer              <- arbitrary
    _glonassPhaseBias_wideLaneInteger      <- arbitraryWord 2
    _glonassPhaseBias_discontinuityCounter <- arbitraryWord 4
    _glonassPhaseBias_phaseBias            <- arbitraryInt 20
    pure GlonassPhaseBias {..}

instance Arbitrary GlonassPhaseBiasCorrection where
  arbitrary = do
    _glonassPhaseBiasCorrection_sat         <- arbitraryWord 5
    _glonassPhaseBiasCorrection_n           <- arbitraryWord 5
    _glonassPhaseBiasCorrection_yawAngle    <- arbitraryWord 9
    _glonassPhaseBiasCorrection_yawRate     <- arbitraryInt 8
    _glonassPhaseBiasCorrection_phaseBiases <- replicateM (fromIntegral _glonassPhaseBiasCorrection_n) arbitrary
    pure GlonassPhaseBiasCorrection {..}

instance Arbitrary Msg1057 where
  arbitrary = do
    _msg1057_header      <- arbitrary
    _msg1057_corrections <- replicateM (fromIntegral $ _msg1057_header ^. gpsOrbitCorrectionHeader_n) arbitrary
    pure Msg1057 {..}

instance Arbitrary Msg1058 where
  arbitrary = do
    _msg1058_header      <- arbitrary
    _msg1058_corrections <- replicateM (fromIntegral $ _msg1058_header ^. gpsClockCorrectionHeader_n) arbitrary
    pure Msg1058 {..}

instance Arbitrary Msg1063 where
  arbitrary = do
    _msg1063_header      <- arbitrary
    _msg1063_corrections <- replicateM (fromIntegral $ _msg1063_header ^. glonassOrbitCorrectionHeader_n) arbitrary
    pure Msg1063 {..}

instance Arbitrary Msg1064 where
  arbitrary = do
    _msg1064_header      <- arbitrary
    _msg1064_corrections <- replicateM (fromIntegral $ _msg1064_header ^. glonassClockCorrectionHeader_n) arbitrary
    pure Msg1064 {..}

instance Arbitrary Msg1060 where
  arbitrary = do
    _msg1060_header      <- arbitrary
    _msg1060_corrections <- replicateM (fromIntegral $ _msg1060_header ^. gpsOrbitClockCorrectionHeader_n) arbitrary
    pure Msg1060 {..}

instance Arbitrary Msg1066 where
  arbitrary = do
    _msg1066_header      <- arbitrary
    _msg1066_corrections <- replicateM (fromIntegral $ _msg1066_header ^. glonassOrbitClockCorrectionHeader_n) arbitrary
    pure Msg1066 {..}

instance Arbitrary Msg1059 where
  arbitrary = do
    _msg1059_header      <- arbitrary
    _msg1059_corrections <- replicateM (fromIntegral $ _msg1059_header ^. gpsCodeBiasCorrectionHeader_n) arbitrary
    pure Msg1059 {..}

instance Arbitrary Msg1065 where
  arbitrary = do
    _msg1065_header      <- arbitrary
    _msg1065_corrections <- replicateM (fromIntegral $ _msg1065_header ^. glonassCodeBiasCorrectionHeader_n) arbitrary
    pure Msg1065 {..}

instance Arbitrary Msg1265 where
  arbitrary = do
    _msg1265_header      <- arbitrary
    _msg1265_corrections <- replicateM (fromIntegral $ _msg1265_header ^. gpsPhaseBiasCorrectionHeader_n) arbitrary
    pure Msg1265 {..}

instance Arbitrary Msg1266 where
  arbitrary = do
    _msg1266_header      <- arbitrary
    _msg1266_corrections <- replicateM (fromIntegral $ _msg1266_header ^. glonassPhaseBiasCorrectionHeader_n) arbitrary
    pure Msg1266 {..}

testMsg1057 :: TestTree
testMsg1057 =
  testProperty "Roundtrip Msg1057" $ \m ->
    decode (encode m) == (m :: Msg1057)

testMsg1058 :: TestTree
testMsg1058 =
  testProperty "Roundtrip Msg1058" $ \m ->
    decode (encode m) == (m :: Msg1058)

testMsg1063 :: TestTree
testMsg1063 =
  testProperty "Roundtrip Msg1063" $ \m ->
    decode (encode m) == (m :: Msg1063)

testMsg1064 :: TestTree
testMsg1064 =
  testProperty "Roundtrip Msg1064" $ \m ->
    decode (encode m) == (m :: Msg1064)

testMsg1060 :: TestTree
testMsg1060 =
  testProperty "Roundtrip Msg1060" $ \m ->
    decode (encode m) == (m :: Msg1060)

testMsg1066 :: TestTree
testMsg1066 =
  testProperty "Roundtrip Msg1066" $ \m ->
    decode (encode m) == (m :: Msg1066)

testMsg1059 :: TestTree
testMsg1059 =
  testProperty "Roundtrip Msg1059" $ \m ->
    decode (encode m) == (m :: Msg1059)

testMsg1065 :: TestTree
testMsg1065 =
  testProperty "Roundtrip Msg1065" $ \m ->
    decode (encode m) == (m :: Msg1065)

testMsg1265 :: TestTree
testMsg1265 =
  testProperty "Roundtrip Msg1265" $ \m ->
    decode (encode m) == (m :: Msg1265)

testMsg1266 :: TestTree
testMsg1266 =
  testProperty "Roundtrip Msg1266" $ \m ->
    decode (encode m) == (m :: Msg1266)

tests :: TestTree
tests =
  testGroup "SSR tests"
    [ testMsg1057
    , testMsg1058
    , testMsg1063
    , testMsg1064
    , testMsg1060
    , testMsg1066
    , testMsg1059
    , testMsg1065
    , testMsg1265
    , testMsg1266
    ]
