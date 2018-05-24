{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS -fno-warn-orphans  #-}

-- |
-- Module:      Test.Data.RTCM3.MSM
-- Copyright:   (c) 2015 Swift Navigation
-- License:     BSD3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
--
-- Test MSM module for RTCM3.

module Test.Data.RTCM3.MSM
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

instance Arbitrary MsmHeader where
  arbitrary = do
    _msmHeader_num               <- arbitraryWord 12
    _msmHeader_station           <- arbitraryWord 12
    _msmHeader_epoch             <- arbitraryWord 30
    _msmHeader_multiple          <- arbitrary
    _msmHeader_iods              <- arbitraryWord 3
    _msmHeader_reserved          <- arbitraryWord 7
    _msmHeader_clockSteering     <- arbitraryWord 2
    _msmHeader_externalClock     <- arbitraryWord 2
    _msmHeader_smoothing         <- arbitrary
    _msmHeader_smoothingInterval <- arbitraryWord 3
    _msmHeader_satelliteMask     <- arbitraryWord 64
    _msmHeader_signalMask        <- arbitraryWord 32
    let x = min 64 $ popCount _msmHeader_satelliteMask * popCount _msmHeader_signalMask
    _msmHeader_cellMask          <- arbitraryWord x
    pure MsmHeader {..}

arbitraryMsm46SatelliteData :: Int -> Gen Msm46SatelliteData
arbitraryMsm46SatelliteData n = do
  _msm46SatelliteData_roughRanges       <- replicateM n $ arbitraryWord 8
  _msm46SatelliteData_roughRangesModulo <- replicateM n $ arbitraryWord 10
  pure Msm46SatelliteData {..}

instance Arbitrary Msg1074 where
  arbitrary = do
    _msg1074_header        <- arbitrary
    _msg1074_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1074_header ^. msmHeader_satelliteMask
    pure Msg1074 {..}

instance Arbitrary Msg1075 where
  arbitrary = do
    _msg1075_header <- arbitrary
    pure Msg1075 {..}

instance Arbitrary Msg1076 where
  arbitrary = do
    _msg1076_header        <- arbitrary
    _msg1076_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1076_header ^. msmHeader_satelliteMask
    pure Msg1076 {..}

instance Arbitrary Msg1077 where
  arbitrary = do
    _msg1077_header <- arbitrary
    pure Msg1077 {..}

instance Arbitrary Msg1084 where
  arbitrary = do
    _msg1084_header <- arbitrary
    _msg1084_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1084_header ^. msmHeader_satelliteMask
    pure Msg1084 {..}

instance Arbitrary Msg1085 where
  arbitrary = do
    _msg1085_header <- arbitrary
    pure Msg1085 {..}

instance Arbitrary Msg1086 where
  arbitrary = do
    _msg1086_header        <- arbitrary
    _msg1086_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1086_header ^. msmHeader_satelliteMask
    pure Msg1086 {..}

instance Arbitrary Msg1087 where
  arbitrary = do
    _msg1087_header <- arbitrary
    pure Msg1087 {..}

instance Arbitrary Msg1094 where
  arbitrary = do
    _msg1094_header        <- arbitrary
    _msg1094_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1094_header ^. msmHeader_satelliteMask
    pure Msg1094 {..}

instance Arbitrary Msg1095 where
  arbitrary = do
    _msg1095_header <- arbitrary
    pure Msg1095 {..}

instance Arbitrary Msg1096 where
  arbitrary = do
    _msg1096_header        <- arbitrary
    _msg1096_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1096_header ^. msmHeader_satelliteMask
    pure Msg1096 {..}

instance Arbitrary Msg1097 where
  arbitrary = do
    _msg1097_header <- arbitrary
    pure Msg1097 {..}

instance Arbitrary Msg1104 where
  arbitrary = do
    _msg1104_header        <- arbitrary
    _msg1104_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1104_header ^. msmHeader_satelliteMask
    pure Msg1104 {..}

instance Arbitrary Msg1105 where
  arbitrary = do
    _msg1105_header <- arbitrary
    pure Msg1105 {..}

instance Arbitrary Msg1106 where
  arbitrary = do
    _msg1106_header        <- arbitrary
    _msg1106_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1106_header ^. msmHeader_satelliteMask
    pure Msg1106 {..}

instance Arbitrary Msg1107 where
  arbitrary = do
    _msg1107_header <- arbitrary
    pure Msg1107 {..}

instance Arbitrary Msg1114 where
  arbitrary = do
    _msg1114_header        <- arbitrary
    _msg1114_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1114_header ^. msmHeader_satelliteMask
    pure Msg1114 {..}

instance Arbitrary Msg1115 where
  arbitrary = do
    _msg1115_header <- arbitrary
    pure Msg1115 {..}

instance Arbitrary Msg1116 where
  arbitrary = do
    _msg1116_header        <- arbitrary
    _msg1116_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1116_header ^. msmHeader_satelliteMask
    pure Msg1116 {..}

instance Arbitrary Msg1117 where
  arbitrary = do
    _msg1117_header <- arbitrary
    pure Msg1117 {..}

instance Arbitrary Msg1124 where
  arbitrary = do
    _msg1124_header        <- arbitrary
    _msg1124_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1124_header ^. msmHeader_satelliteMask
    pure Msg1124 {..}

instance Arbitrary Msg1125 where
  arbitrary = do
    _msg1125_header <- arbitrary
    pure Msg1125 {..}

instance Arbitrary Msg1126 where
  arbitrary = do
    _msg1126_header        <- arbitrary
    _msg1126_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1126_header ^. msmHeader_satelliteMask
    pure Msg1126 {..}

instance Arbitrary Msg1127 where
  arbitrary = do
    _msg1127_header <- arbitrary
    pure Msg1127 {..}

testMsg1074 :: TestTree
testMsg1074 =
  testProperty "Roundtrip Msg1074" $ \m ->
    decode (encode m) == (m :: Msg1074)

testMsg1075 :: TestTree
testMsg1075 =
  testProperty "Roundtrip Msg1075" $ \m ->
    decode (encode m) == (m :: Msg1075)

testMsg1076 :: TestTree
testMsg1076 =
  testProperty "Roundtrip Msg1076" $ \m ->
    decode (encode m) == (m :: Msg1076)

testMsg1077 :: TestTree
testMsg1077 =
  testProperty "Roundtrip Msg1077" $ \m ->
    decode (encode m) == (m :: Msg1077)

testMsg1084 :: TestTree
testMsg1084 =
  testProperty "Roundtrip Msg1084" $ \m ->
    decode (encode m) == (m :: Msg1084)

testMsg1085 :: TestTree
testMsg1085 =
  testProperty "Roundtrip Msg1085" $ \m ->
    decode (encode m) == (m :: Msg1085)

testMsg1086 :: TestTree
testMsg1086 =
  testProperty "Roundtrip Msg1086" $ \m ->
    decode (encode m) == (m :: Msg1086)

testMsg1087 :: TestTree
testMsg1087 =
  testProperty "Roundtrip Msg1087" $ \m ->
    decode (encode m) == (m :: Msg1087)

testMsg1094 :: TestTree
testMsg1094 =
  testProperty "Roundtrip Msg1094" $ \m ->
    decode (encode m) == (m :: Msg1094)

testMsg1095 :: TestTree
testMsg1095 =
  testProperty "Roundtrip Msg1095" $ \m ->
    decode (encode m) == (m :: Msg1095)

testMsg1096 :: TestTree
testMsg1096 =
  testProperty "Roundtrip Msg1096" $ \m ->
    decode (encode m) == (m :: Msg1096)

testMsg1097 :: TestTree
testMsg1097 =
  testProperty "Roundtrip Msg1097" $ \m ->
    decode (encode m) == (m :: Msg1097)

testMsg1104 :: TestTree
testMsg1104 =
  testProperty "Roundtrip Msg1104" $ \m ->
    decode (encode m) == (m :: Msg1104)

testMsg1105 :: TestTree
testMsg1105 =
  testProperty "Roundtrip Msg1105" $ \m ->
    decode (encode m) == (m :: Msg1105)

testMsg1106 :: TestTree
testMsg1106 =
  testProperty "Roundtrip Msg1106" $ \m ->
    decode (encode m) == (m :: Msg1106)

testMsg1107 :: TestTree
testMsg1107 =
  testProperty "Roundtrip Msg1107" $ \m ->
    decode (encode m) == (m :: Msg1107)

testMsg1114 :: TestTree
testMsg1114 =
  testProperty "Roundtrip Msg1114" $ \m ->
    decode (encode m) == (m :: Msg1114)

testMsg1115 :: TestTree
testMsg1115 =
  testProperty "Roundtrip Msg1115" $ \m ->
    decode (encode m) == (m :: Msg1115)

testMsg1116 :: TestTree
testMsg1116 =
  testProperty "Roundtrip Msg1116" $ \m ->
    decode (encode m) == (m :: Msg1116)

testMsg1117 :: TestTree
testMsg1117 =
  testProperty "Roundtrip Msg1117" $ \m ->
    decode (encode m) == (m :: Msg1117)

testMsg1124 :: TestTree
testMsg1124 =
  testProperty "Roundtrip Msg1124" $ \m ->
    decode (encode m) == (m :: Msg1124)

testMsg1125 :: TestTree
testMsg1125 =
  testProperty "Roundtrip Msg1125" $ \m ->
    decode (encode m) == (m :: Msg1125)

testMsg1126 :: TestTree
testMsg1126 =
  testProperty "Roundtrip Msg1126" $ \m ->
    decode (encode m) == (m :: Msg1126)

testMsg1127 :: TestTree
testMsg1127 =
  testProperty "Roundtrip Msg1127" $ \m ->
    decode (encode m) == (m :: Msg1127)

tests :: TestTree
tests =
  testGroup "MSM tests"
    [ testMsg1074
    , testMsg1075
    , testMsg1076
    , testMsg1077
    , testMsg1084
    , testMsg1085
    , testMsg1086
    , testMsg1087
    , testMsg1094
    , testMsg1095
    , testMsg1096
    , testMsg1097
    , testMsg1104
    , testMsg1105
    , testMsg1106
    , testMsg1107
    , testMsg1114
    , testMsg1115
    , testMsg1116
    , testMsg1117
    , testMsg1124
    , testMsg1125
    , testMsg1126
    , testMsg1127
    ]
