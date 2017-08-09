{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS -fno-warn-orphans  #-}

-- |
-- Module:      Test.Data.RTCM3.Antennas
-- Copyright:   (c) 2015 Swift Navigation
-- License:     BSD3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
--
-- Test Antennas module for RTCM3.

module Test.Data.RTCM3.Antennas
  ( tests
  ) where

import BasicPrelude
import Data.Binary
import Data.RTCM3
import Test.Data.RTCM3.Test
import Test.Tasty
import Test.Tasty.QuickCheck

instance Arbitrary AntennaReference where
  arbitrary = do
    _antennaReference_num          <- arbitraryWord 12
    _antennaReference_station      <- arbitraryWord 12
    _antennaReference_gps          <- arbitrary
    _antennaReference_glonass      <- arbitrary
    _antennaReference_computed     <- arbitrary
    _antennaReference_ecef_x       <- arbitraryInt 38
    _antennaReference_oscillator   <- arbitrary
    _antennaReference_ecef_y       <- arbitraryInt 38
    _antennaReference_quarterCycle <- arbitraryWord 2
    _antennaReference_ecef_z       <- arbitraryInt 38
    pure AntennaReference {..}

instance Arbitrary ExtAntennaReference where
  arbitrary = do
    _extAntennaReference_height <- arbitraryWord 16
    pure ExtAntennaReference {..}

instance Arbitrary AntennaDescriptor where
  arbitrary = do
    _antennaDescriptor_num         <- arbitraryWord 12
    _antennaDescriptor_station     <- arbitraryWord 12
    _antennaDescriptor_n           <- arbitraryWord 8
    _antennaDescriptor_descriptors <- replicateM (fromIntegral _antennaDescriptor_n) $ arbitraryWord 8
    _antennaDescriptor_setup       <- arbitraryWord 8
    pure AntennaDescriptor {..}

instance Arbitrary ExtAntennaDescriptor where
  arbitrary = do
    _extAntennaDescriptor_n <- arbitraryWord 8
    _extAntennaDescriptor_serialNumbers <- replicateM (fromIntegral _extAntennaDescriptor_n) $ arbitraryWord 8
    pure ExtAntennaDescriptor {..}

instance Arbitrary ReceiverDescriptor where
  arbitrary = do
    _receiverDescriptor_n                <- arbitraryWord 8
    _receiverDescriptor_descriptors      <- replicateM (fromIntegral _receiverDescriptor_n) $ arbitraryWord 8
    _receiverDescriptor_m                <- arbitraryWord 8
    _receiverDescriptor_firmwareVersions <- replicateM (fromIntegral _receiverDescriptor_m) $ arbitraryWord 8
    _receiverDescriptor_l                <- arbitraryWord 8
    _receiverDescriptor_serialNumbers    <- replicateM (fromIntegral _receiverDescriptor_l) $ arbitraryWord 8
    pure ReceiverDescriptor {..}

instance Arbitrary Msg1005 where
  arbitrary = Msg1005 <$> arbitrary

instance Arbitrary Msg1006 where
  arbitrary = Msg1006 <$> arbitrary <*> arbitrary

instance Arbitrary Msg1007 where
  arbitrary = Msg1007 <$> arbitrary

instance Arbitrary Msg1008 where
  arbitrary = Msg1008 <$> arbitrary <*> arbitrary

instance Arbitrary Msg1033 where
  arbitrary = Msg1033 <$> arbitrary <*> arbitrary <*> arbitrary

testMsg1005 :: TestTree
testMsg1005 =
  testProperty "Roundtrip Msg1005" $ \m ->
    decode (encode m) == (m :: Msg1005)

testMsg1006 :: TestTree
testMsg1006 =
  testProperty "Roundtrip Msg1006" $ \m ->
    decode (encode m) == (m :: Msg1006)

testMsg1007 :: TestTree
testMsg1007 =
  testProperty "Roundtrip Msg1007" $ \m ->
    decode (encode m) == (m :: Msg1007)

testMsg1008 :: TestTree
testMsg1008 =
  testProperty "Roundtrip Msg1008" $ \m ->
    decode (encode m) == (m :: Msg1008)

testMsg1033 :: TestTree
testMsg1033 =
  testProperty "Roundtrip Msg1033" $ \m ->
    decode (encode m) == (m :: Msg1033)

tests :: TestTree
tests =
  testGroup "Antennas tests"
    [ testMsg1005
    , testMsg1006
    , testMsg1007
    , testMsg1008
    , testMsg1033
    ]
