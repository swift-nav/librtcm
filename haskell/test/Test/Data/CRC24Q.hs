{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Test.Data.CRC24Q
-- Copyright:   (c) 2015 Swift Navigation
-- License:     BSD3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
--
-- Test module for CRC24Q.

module Test.Data.CRC24Q
  ( tests
  ) where

import BasicPrelude
import Data.CRC24Q
import Test.Tasty
import Test.Tasty.HUnit

testEmpty :: TestTree
testEmpty = testCase "Empty test" $
  crc24q "" @?= 0

testSimple :: TestTree
testSimple = testCase "Simple test" $
  crc24q "123456789" @?= 0xCDE703

tests :: TestTree
tests =
  testGroup "CRC24Q tests"
    [ testEmpty
    , testSimple
    ]
