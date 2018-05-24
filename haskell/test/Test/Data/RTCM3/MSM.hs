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
  _msm46SatelliteData_ranges       <- replicateM n $ arbitraryWord 8
  _msm46SatelliteData_rangesModulo <- replicateM n $ arbitraryWord 10
  pure Msm46SatelliteData {..}

arbitraryMsm57SatelliteData :: Int -> Gen Msm57SatelliteData
arbitraryMsm57SatelliteData n = do
  _msm57SatelliteData_ranges          <- replicateM n $ arbitraryWord 8
  _msm57SatelliteData_extendeds       <- replicateM n $ arbitraryWord 4
  _msm57SatelliteData_rangesModulo    <- replicateM n $ arbitraryWord 10
  _msm57SatelliteData_phaseRangeRates <- replicateM n $ arbitraryWord 14
  pure Msm57SatelliteData {..}

arbitraryMsm4SignalData :: Int -> Gen Msm4SignalData
arbitraryMsm4SignalData n = do
  _msm4SignalData_pseudoranges <- replicateM n $ arbitraryInt 15
  _msm4SignalData_phaseranges  <- replicateM n $ arbitraryInt 22
  _msm4SignalData_lockTimes    <- replicateM n $ arbitraryWord 4
  _msm4SignalData_halfCycles   <- replicateM n arbitrary
  _msm4SignalData_cnrs         <- replicateM n $ arbitraryWord 6
  pure Msm4SignalData {..}

arbitraryMsm5SignalData :: Int -> Gen Msm5SignalData
arbitraryMsm5SignalData n = do
  _msm5SignalData_pseudoranges    <- replicateM n $ arbitraryInt 15
  _msm5SignalData_phaseranges     <- replicateM n $ arbitraryInt 22
  _msm5SignalData_lockTimes       <- replicateM n $ arbitraryWord 4
  _msm5SignalData_halfCycles      <- replicateM n arbitrary
  _msm5SignalData_cnrs            <- replicateM n $ arbitraryWord 6
  _msm5SignalData_phaseRangeRates <- replicateM n $ arbitraryInt 15
  pure Msm5SignalData {..}

arbitraryMsm6SignalData :: Int -> Gen Msm6SignalData
arbitraryMsm6SignalData n = do
  _msm6SignalData_pseudoranges <- replicateM n $ arbitraryInt 20
  _msm6SignalData_phaseranges  <- replicateM n $ arbitraryInt 24
  _msm6SignalData_lockTimes    <- replicateM n $ arbitraryWord 10
  _msm6SignalData_halfCycles   <- replicateM n arbitrary
  _msm6SignalData_cnrs         <- replicateM n $ arbitraryWord 10
  pure Msm6SignalData {..}

arbitraryMsm7SignalData :: Int -> Gen Msm7SignalData
arbitraryMsm7SignalData n = do
  _msm7SignalData_pseudoranges    <- replicateM n $ arbitraryInt 20
  _msm7SignalData_phaseranges     <- replicateM n $ arbitraryInt 24
  _msm7SignalData_lockTimes       <- replicateM n $ arbitraryWord 10
  _msm7SignalData_halfCycles      <- replicateM n arbitrary
  _msm7SignalData_cnrs            <- replicateM n $ arbitraryWord 10
  _msm7SignalData_phaseRangeRates <- replicateM n $ arbitraryInt 15
  pure Msm7SignalData {..}

instance Arbitrary Msg1074 where
  arbitrary = do
    _msg1074_header        <- arbitrary
    _msg1074_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1074_header ^. msmHeader_satelliteMask
    _msg1074_signalData    <- arbitraryMsm4SignalData $ popCount $ _msg1074_header ^. msmHeader_cellMask
    pure Msg1074 {..}

instance Arbitrary Msg1075 where
  arbitrary = do
    _msg1075_header        <- arbitrary
    _msg1075_satelliteData <- arbitraryMsm57SatelliteData $ popCount $ _msg1075_header ^. msmHeader_satelliteMask
    _msg1075_signalData    <- arbitraryMsm5SignalData $ popCount $ _msg1075_header ^. msmHeader_cellMask
    pure Msg1075 {..}

instance Arbitrary Msg1076 where
  arbitrary = do
    _msg1076_header        <- arbitrary
    _msg1076_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1076_header ^. msmHeader_satelliteMask
    _msg1076_signalData    <- arbitraryMsm6SignalData $ popCount $ _msg1076_header ^. msmHeader_cellMask
    pure Msg1076 {..}

instance Arbitrary Msg1077 where
  arbitrary = do
    _msg1077_header        <- arbitrary
    _msg1077_satelliteData <- arbitraryMsm57SatelliteData $ popCount $ _msg1077_header ^. msmHeader_satelliteMask
    _msg1077_signalData    <- arbitraryMsm7SignalData $ popCount $ _msg1077_header ^. msmHeader_cellMask
    pure Msg1077 {..}

instance Arbitrary Msg1084 where
  arbitrary = do
    _msg1084_header        <- arbitrary
    _msg1084_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1084_header ^. msmHeader_satelliteMask
    _msg1084_signalData    <- arbitraryMsm4SignalData $ popCount $ _msg1084_header ^. msmHeader_cellMask
    pure Msg1084 {..}

instance Arbitrary Msg1085 where
  arbitrary = do
    _msg1085_header        <- arbitrary
    _msg1085_satelliteData <- arbitraryMsm57SatelliteData $ popCount $ _msg1085_header ^. msmHeader_satelliteMask
    _msg1085_signalData    <- arbitraryMsm5SignalData $ popCount $ _msg1085_header ^. msmHeader_cellMask
    pure Msg1085 {..}

instance Arbitrary Msg1086 where
  arbitrary = do
    _msg1086_header        <- arbitrary
    _msg1086_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1086_header ^. msmHeader_satelliteMask
    _msg1086_signalData    <- arbitraryMsm6SignalData $ popCount $ _msg1086_header ^. msmHeader_cellMask
    pure Msg1086 {..}

instance Arbitrary Msg1087 where
  arbitrary = do
    _msg1087_header        <- arbitrary
    _msg1087_satelliteData <- arbitraryMsm57SatelliteData $ popCount $ _msg1087_header ^. msmHeader_satelliteMask
    _msg1087_signalData    <- arbitraryMsm7SignalData $ popCount $ _msg1087_header ^. msmHeader_cellMask
    pure Msg1087 {..}

instance Arbitrary Msg1094 where
  arbitrary = do
    _msg1094_header        <- arbitrary
    _msg1094_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1094_header ^. msmHeader_satelliteMask
    _msg1094_signalData    <- arbitraryMsm4SignalData $ popCount $ _msg1094_header ^. msmHeader_cellMask
    pure Msg1094 {..}

instance Arbitrary Msg1095 where
  arbitrary = do
    _msg1095_header        <- arbitrary
    _msg1095_satelliteData <- arbitraryMsm57SatelliteData $ popCount $ _msg1095_header ^. msmHeader_satelliteMask
    _msg1095_signalData    <- arbitraryMsm5SignalData $ popCount $ _msg1095_header ^. msmHeader_cellMask
    pure Msg1095 {..}

instance Arbitrary Msg1096 where
  arbitrary = do
    _msg1096_header        <- arbitrary
    _msg1096_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1096_header ^. msmHeader_satelliteMask
    _msg1096_signalData    <- arbitraryMsm6SignalData $ popCount $ _msg1096_header ^. msmHeader_cellMask
    pure Msg1096 {..}

instance Arbitrary Msg1097 where
  arbitrary = do
    _msg1097_header        <- arbitrary
    _msg1097_satelliteData <- arbitraryMsm57SatelliteData $ popCount $ _msg1097_header ^. msmHeader_satelliteMask
    _msg1097_signalData    <- arbitraryMsm7SignalData $ popCount $ _msg1097_header ^. msmHeader_cellMask
    pure Msg1097 {..}

instance Arbitrary Msg1104 where
  arbitrary = do
    _msg1104_header        <- arbitrary
    _msg1104_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1104_header ^. msmHeader_satelliteMask
    _msg1104_signalData    <- arbitraryMsm4SignalData $ popCount $ _msg1104_header ^. msmHeader_cellMask
    pure Msg1104 {..}

instance Arbitrary Msg1105 where
  arbitrary = do
    _msg1105_header        <- arbitrary
    _msg1105_satelliteData <- arbitraryMsm57SatelliteData $ popCount $ _msg1105_header ^. msmHeader_satelliteMask
    _msg1105_signalData    <- arbitraryMsm5SignalData $ popCount $ _msg1105_header ^. msmHeader_cellMask
    pure Msg1105 {..}

instance Arbitrary Msg1106 where
  arbitrary = do
    _msg1106_header        <- arbitrary
    _msg1106_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1106_header ^. msmHeader_satelliteMask
    _msg1106_signalData    <- arbitraryMsm6SignalData $ popCount $ _msg1106_header ^. msmHeader_cellMask
    pure Msg1106 {..}

instance Arbitrary Msg1107 where
  arbitrary = do
    _msg1107_header        <- arbitrary
    _msg1107_satelliteData <- arbitraryMsm57SatelliteData $ popCount $ _msg1107_header ^. msmHeader_satelliteMask
    _msg1107_signalData    <- arbitraryMsm7SignalData $ popCount $ _msg1107_header ^. msmHeader_cellMask
    pure Msg1107 {..}

instance Arbitrary Msg1114 where
  arbitrary = do
    _msg1114_header        <- arbitrary
    _msg1114_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1114_header ^. msmHeader_satelliteMask
    _msg1114_signalData    <- arbitraryMsm4SignalData $ popCount $ _msg1114_header ^. msmHeader_cellMask
    pure Msg1114 {..}

instance Arbitrary Msg1115 where
  arbitrary = do
    _msg1115_header        <- arbitrary
    _msg1115_satelliteData <- arbitraryMsm57SatelliteData $ popCount $ _msg1115_header ^. msmHeader_satelliteMask
    _msg1115_signalData    <- arbitraryMsm5SignalData $ popCount $ _msg1115_header ^. msmHeader_cellMask
    pure Msg1115 {..}

instance Arbitrary Msg1116 where
  arbitrary = do
    _msg1116_header        <- arbitrary
    _msg1116_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1116_header ^. msmHeader_satelliteMask
    _msg1116_signalData    <- arbitraryMsm6SignalData $ popCount $ _msg1116_header ^. msmHeader_cellMask
    pure Msg1116 {..}

instance Arbitrary Msg1117 where
  arbitrary = do
    _msg1117_header        <- arbitrary
    _msg1117_satelliteData <- arbitraryMsm57SatelliteData $ popCount $ _msg1117_header ^. msmHeader_satelliteMask
    _msg1117_signalData    <- arbitraryMsm7SignalData $ popCount $ _msg1117_header ^. msmHeader_cellMask
    pure Msg1117 {..}

instance Arbitrary Msg1124 where
  arbitrary = do
    _msg1124_header        <- arbitrary
    _msg1124_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1124_header ^. msmHeader_satelliteMask
    _msg1124_signalData    <- arbitraryMsm4SignalData $ popCount $ _msg1124_header ^. msmHeader_cellMask
    pure Msg1124 {..}

instance Arbitrary Msg1125 where
  arbitrary = do
    _msg1125_header       <- arbitrary
    _msg1125_satelliteData <- arbitraryMsm57SatelliteData $ popCount $ _msg1125_header ^. msmHeader_satelliteMask
    _msg1125_signalData    <- arbitraryMsm5SignalData $ popCount $ _msg1125_header ^. msmHeader_cellMask
    pure Msg1125 {..}

instance Arbitrary Msg1126 where
  arbitrary = do
    _msg1126_header        <- arbitrary
    _msg1126_satelliteData <- arbitraryMsm46SatelliteData $ popCount $ _msg1126_header ^. msmHeader_satelliteMask
    _msg1126_signalData    <- arbitraryMsm6SignalData $ popCount $ _msg1126_header ^. msmHeader_cellMask
    pure Msg1126 {..}

instance Arbitrary Msg1127 where
  arbitrary = do
    _msg1127_header        <- arbitrary
    _msg1127_satelliteData <- arbitraryMsm57SatelliteData $ popCount $ _msg1127_header ^. msmHeader_satelliteMask
    _msg1127_signalData    <- arbitraryMsm7SignalData $ popCount $ _msg1127_header ^. msmHeader_cellMask
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
