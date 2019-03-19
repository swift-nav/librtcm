{-# OPTIONS -fno-warn-orphans  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module:      Test.Data.RTCM3.System
-- Copyright:   (c) 2015 Swift Navigation
-- License:     BSD3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
--
-- Test System module for RTCM3.

module Test.Data.RTCM3.System
  ( tests
  ) where

import BasicPrelude
import Control.Lens
import Data.Binary
import Data.RTCM3
import Data.Text
import Test.Data.RTCM3.Test
import Test.Tasty
import Test.Tasty.QuickCheck

instance Arbitrary MessageHeader where
  arbitrary = do
    _messageHeader_num         <- arbitraryWord 12
    _messageHeader_station     <- arbitraryWord 12
    _messageHeader_mjd         <- arbitraryWord 16
    _messageHeader_seconds     <- arbitraryWord 17
    _messageHeader_n           <- arbitraryWord 5
    _messageHeader_leapSeconds <- arbitraryWord 8
    pure MessageHeader {..}

instance Arbitrary Message where
  arbitrary = do
    _message_num         <- arbitraryWord 12
    _message_synchronous <- arbitrary
    _message_interval    <- arbitraryWord 16
    pure Message {..}

instance Arbitrary TextMessage where
  arbitrary = do
    _textMessage_num        <- arbitraryWord 12
    _textMessage_station    <- arbitraryWord 12
    _textMessage_mjd        <- arbitraryWord 16
    _textMessage_seconds    <- arbitraryWord 17
    _textMessage_characters <- arbitraryWord 7
    _textMessage_n          <- arbitraryWord 8
    _textMessage_text       <- pack <$> replicateM (fromIntegral _textMessage_n) arbitrary
    pure TextMessage {..}

instance Arbitrary Msg1013 where
  arbitrary = do
    header   <- arbitrary
    messages <- replicateM (fromIntegral $ header ^. messageHeader_n) arbitrary
    pure $ Msg1013 header messages

instance Arbitrary Msg1029 where
  arbitrary =
    Msg1029 <$> arbitrary

testMsg1013 :: TestTree
testMsg1013 =
  testProperty "Roundtrip Msg1013" $ \m ->
    decode (encode m) == (m :: Msg1013)

tests :: TestTree
tests =
  testGroup "System tests"
    [ testMsg1013
    ]
