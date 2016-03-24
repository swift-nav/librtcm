-- |
-- Module:      Test.Data.RTCM3.Test
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Test module for RTCM3.

module Test.Data.RTCM3.Test where

import BasicPrelude
import Data.Bits
import Test.Tasty.QuickCheck
import System.Random

arbitraryWord :: (Integral a, Bits a, Random a) => Int -> Gen a
arbitraryWord b = do
  let m = (1 `shiftL` b) - 1
  choose (0, m)

arbitraryInt :: (Integral a, Bits a, Random a) => Int -> Gen a
arbitraryInt b = do
  let m = (1 `shiftL` (b - 1)) - 1
  choose (-m, m)
