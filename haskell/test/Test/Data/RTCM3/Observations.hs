{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS -fno-warn-orphans  #-}

-- |
-- Module:      Test.Data.RTCM3.Observations
-- Copyright:   (c) 2015 Swift Navigation
-- License:     BSD3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
--
-- Test Observations module for RTCM3.

module Test.Data.RTCM3.Observations
  ( tests
  ) where

import BasicPrelude
import Control.Lens
import Data.Binary
import Data.Bits
import Data.RTCM3
import Test.Data.RTCM3.Test
import Test.Tasty
import Test.Tasty.QuickCheck

instance Arbitrary GpsObservationHeader where
  arbitrary = do
    _gpsObservationHeader_num               <- arbitraryWord 12
    _gpsObservationHeader_station           <- arbitraryWord 12
    _gpsObservationHeader_tow               <- arbitraryWord 30
    _gpsObservationHeader_synchronous       <- arbitrary
    _gpsObservationHeader_n                 <- arbitraryWord 5
    _gpsObservationHeader_smoothing         <- arbitrary
    _gpsObservationHeader_smoothingInterval <- arbitraryWord 3
    pure GpsObservationHeader {..}

instance Arbitrary GpsL1Observation where
  arbitrary = do
    _gpsL1Observation_code             <- arbitrary
    _gpsL1Observation_pseudorange      <- arbitraryWord 24
    _gpsL1Observation_carrierMinusCode <- arbitraryInt 20
    _gpsL1Observation_lockTime         <- arbitraryWord 7
    pure GpsL1Observation {..}

instance Arbitrary GpsL1ExtObservation where
  arbitrary = do
    _gpsL1ExtObservation_ambiguity <- arbitraryWord 8
    _gpsL1ExtObservation_cnr       <- arbitraryWord 8
    pure GpsL1ExtObservation {..}

instance Arbitrary GpsL2Observation where
  arbitrary = do
    _gpsL2Observation_code                  <- arbitraryWord 2
    _gpsL2Observation_pseudorangeDifference <- arbitraryInt 14
    _gpsL2Observation_carrierMinusCode      <- arbitraryInt 20
    _gpsL2Observation_lockTime              <- arbitraryWord 7
    pure GpsL2Observation {..}

instance Arbitrary GpsL2ExtObservation where
  arbitrary = do
    _gpsL2ExtObservation_cnr <- arbitraryWord 8
    pure GpsL2ExtObservation {..}

instance Arbitrary GlonassObservationHeader where
  arbitrary = do
    _glonassObservationHeader_num               <- arbitraryWord 12
    _glonassObservationHeader_station           <- arbitraryWord 12
    _glonassObservationHeader_epoch             <- arbitraryWord 27
    _glonassObservationHeader_synchronous       <- arbitrary
    _glonassObservationHeader_n                 <- arbitraryWord 5
    _glonassObservationHeader_smoothing         <- arbitrary
    _glonassObservationHeader_smoothingInterval <- arbitraryWord 3
    pure GlonassObservationHeader {..}

instance Arbitrary GlonassL1Observation where
  arbitrary = do
    _glonassL1Observation_code             <- arbitrary
    _glonassL1Observation_frequency        <- arbitraryWord 5
    _glonassL1Observation_pseudorange      <- arbitraryWord 25
    _glonassL1Observation_carrierMinusCode <- arbitraryInt 20
    _glonassL1Observation_lockTime         <- arbitraryWord 7
    pure GlonassL1Observation {..}

instance Arbitrary GlonassL1ExtObservation where
  arbitrary = do
    _glonassL1ExtObservation_ambiguity <- arbitraryWord 7
    _glonassL1ExtObservation_cnr       <- arbitraryWord 8
    pure GlonassL1ExtObservation {..}

instance Arbitrary GlonassL2Observation where
  arbitrary = do
    _glonassL2Observation_code                  <- arbitraryWord 2
    _glonassL2Observation_pseudorangeDifference <- arbitraryInt 14
    _glonassL2Observation_carrierMinusCode      <- arbitraryInt 20
    _glonassL2Observation_lockTime              <- arbitraryWord 7
    pure GlonassL2Observation {..}

instance Arbitrary GlonassL2ExtObservation where
  arbitrary = do
    _glonassL2ExtObservation_cnr <- arbitraryWord 8
    pure GlonassL2ExtObservation {..}

instance Arbitrary GlonassBias where
  arbitrary = do
    _glonassBias_num     <- arbitraryWord 12
    _glonassBias_station <- arbitraryWord 12
    _glonassBias_bias    <- arbitrary
    _glonassBias_mask    <- arbitraryWord 4
    _glonassBias_biases  <- replicateM (popCount _glonassBias_mask) $ arbitraryInt 16
    pure GlonassBias {..}

instance Arbitrary Observation1001 where
  arbitrary = Observation1001 <$> arbitraryWord 6 <*> arbitrary

instance Arbitrary Msg1001 where
  arbitrary = do
    _msg1001_header       <- arbitrary
    _msg1001_observations <- replicateM (fromIntegral $ _msg1001_header ^. gpsObservationHeader_n) arbitrary
    pure Msg1001 {..}

instance Arbitrary Observation1002 where
  arbitrary = Observation1002 <$> arbitraryWord 6 <*> arbitrary <*> arbitrary

instance Arbitrary Msg1002 where
  arbitrary = do
    _msg1002_header       <- arbitrary
    _msg1002_observations <- replicateM (fromIntegral $ _msg1002_header ^. gpsObservationHeader_n) arbitrary
    pure Msg1002 {..}

instance Arbitrary Observation1003 where
  arbitrary = Observation1003 <$> arbitraryWord 6 <*> arbitrary <*> arbitrary

instance Arbitrary Msg1003 where
  arbitrary = do
    _msg1003_header       <- arbitrary
    _msg1003_observations <- replicateM (fromIntegral $ _msg1003_header ^. gpsObservationHeader_n) arbitrary
    pure Msg1003 {..}

instance Arbitrary Observation1004 where
  arbitrary = Observation1004 <$> arbitraryWord 6 <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Msg1004 where
  arbitrary = do
    _msg1004_header       <- arbitrary
    _msg1004_observations <- replicateM (fromIntegral $ _msg1004_header ^. gpsObservationHeader_n) arbitrary
    pure Msg1004 {..}

instance Arbitrary Observation1009 where
  arbitrary = Observation1009 <$> arbitraryWord 6 <*> arbitrary

instance Arbitrary Msg1009 where
  arbitrary = do
    _msg1009_header       <- arbitrary
    _msg1009_observations <- replicateM (fromIntegral $ _msg1009_header ^. glonassObservationHeader_n) arbitrary
    pure Msg1009 {..}

instance Arbitrary Observation1010 where
  arbitrary = Observation1010 <$> arbitraryWord 6 <*> arbitrary <*> arbitrary

instance Arbitrary Msg1010 where
  arbitrary = do
    _msg1010_header       <- arbitrary
    _msg1010_observations <- replicateM (fromIntegral $ _msg1010_header ^. glonassObservationHeader_n) arbitrary
    pure Msg1010 {..}

instance Arbitrary Observation1011 where
  arbitrary = Observation1011 <$> arbitraryWord 6 <*> arbitrary <*> arbitrary

instance Arbitrary Msg1011 where
  arbitrary = do
    _msg1011_header       <- arbitrary
    _msg1011_observations <- replicateM (fromIntegral $ _msg1011_header ^. glonassObservationHeader_n) arbitrary
    pure Msg1011 {..}

instance Arbitrary Observation1012 where
  arbitrary = Observation1012 <$> arbitraryWord 6 <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Msg1012 where
  arbitrary = do
    _msg1012_header       <- arbitrary
    _msg1012_observations <- replicateM (fromIntegral $ _msg1012_header ^. glonassObservationHeader_n) arbitrary
    pure Msg1012 {..}

instance Arbitrary Msg1230 where
  arbitrary = Msg1230 <$> arbitrary

testMsg1001 :: TestTree
testMsg1001 =
  testProperty "Roundtrip Msg1001" $ \m ->
    decode (encode m) == (m :: Msg1001)

testMsg1002 :: TestTree
testMsg1002 =
  testProperty "Roundtrip Msg1002" $ \m ->
    decode (encode m) == (m :: Msg1002)

testMsg1003 :: TestTree
testMsg1003 =
  testProperty "Roundtrip Msg1003" $ \m ->
    decode (encode m) == (m :: Msg1003)

testMsg1004 :: TestTree
testMsg1004 =
  testProperty "Roundtrip Msg1004" $ \m ->
    decode (encode m) == (m :: Msg1004)

testMsg1009 :: TestTree
testMsg1009 =
  testProperty "Roundtrip Msg1009" $ \m ->
    decode (encode m) == (m :: Msg1009)

testMsg1010 :: TestTree
testMsg1010 =
  testProperty "Roundtrip Msg1010" $ \m ->
    decode (encode m) == (m :: Msg1010)

testMsg1011 :: TestTree
testMsg1011 =
  testProperty "Roundtrip Msg1011" $ \m ->
    decode (encode m) == (m :: Msg1011)

testMsg1012 :: TestTree
testMsg1012 =
  testProperty "Roundtrip Msg1012" $ \m ->
    decode (encode m) == (m :: Msg1012)

testMsg1230 :: TestTree
testMsg1230 =
  testProperty "Roundtrip Msg1230" $ \m ->
    decode (encode m) == (m :: Msg1230)

tests :: TestTree
tests =
  testGroup "Observations tests"
    [ testMsg1001
    , testMsg1002
    , testMsg1003
    , testMsg1004
    , testMsg1009
    , testMsg1010
    , testMsg1011
    , testMsg1012
    , testMsg1230
    ]
