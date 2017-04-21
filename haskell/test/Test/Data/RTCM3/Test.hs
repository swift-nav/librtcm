{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module:      Test.Data.RTCM3.Test
-- Copyright:   (c) 2015 Swift Navigation
-- License:     BSD3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
--
-- Test module for RTCM3.

module Test.Data.RTCM3.Test where

import BasicPrelude
import Data.Bits
import System.Random
import Test.Tasty.QuickCheck

arbitraryWord :: (Integral a, Bits a, Random a) => Int -> Gen a
arbitraryWord b = do
  let m = (1 `shiftL` b) - 1
  choose (0, m)

arbitraryInt :: (Integral a, Bits a, Random a) => Int -> Gen a
arbitraryInt b = do
  let m = (1 `shiftL` (b - 1)) - 1
  choose (-m, m)
