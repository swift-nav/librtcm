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

instance Arbitrary Msg1074 where
  arbitrary = do
    _msg1074_header <- arbitrary
    pure Msg1074 {..}

testMsg1074 :: TestTree
testMsg1074 =
  testProperty "Roundtrip Msg1074" $ \m ->
    decode (encode m) == (m :: Msg1074)

tests :: TestTree
tests =
  testGroup "MSM tests"
    [ testMsg1074
    ]
