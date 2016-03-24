{-# OPTIONS -fno-warn-orphans #-}

module Test.Data.RTCM3.System
  ( tests
  ) where

import BasicPrelude
import Control.Lens
import Data.Binary
import Data.RTCM3
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
    return MessageHeader {..}

instance Arbitrary Message where
  arbitrary = do
    _message_num         <- arbitraryWord 12
    _message_synchronous <- arbitrary
    _message_interval    <- arbitraryWord 16
    return Message {..}

instance Arbitrary Msg1013 where
  arbitrary = do
    header   <- arbitrary
    messages <- replicateM (fromIntegral $ header ^. messageHeader_n) arbitrary
    return $ Msg1013 header messages

testMsg1013 :: TestTree
testMsg1013 =
  testProperty "Roundtrip Msg1013" $ \msg ->
    (decode $ encode msg) == (msg :: Msg1013)

tests :: TestTree
tests =
  testGroup "System tests"
    [ testMsg1013
    ]
