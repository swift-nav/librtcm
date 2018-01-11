{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-orphans  #-}

-- |
-- Module:      Test.Data.RTCM3.Extras
-- Copyright:   (c) 2015 Swift Navigation
-- License:     BSD3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
--
-- Test Extras module for RTCM3.

module Test.Data.RTCM3.Extras
  ( tests
  ) where

import           BasicPrelude          hiding (ByteString)
import           Data.Binary
import qualified Data.Binary.Bits.Get  as B
import qualified Data.Binary.Bits.Put  as B
import qualified Data.Binary.Get       as G
import qualified Data.Binary.Put       as P
import           Data.Bits
import           Data.ByteString.Lazy
import           Data.Int
import           Data.RTCM3.Internal
import           Data.Word.Word24
import           System.Random
import           Test.Data.RTCM3.Test
import           Test.Tasty
import           Test.Tasty.QuickCheck

data TestInt a = TestInt Int a deriving ( Show, Read, Eq )

arbitraryTestInt :: (Integral a, Bits a, Random a) => Int -> Gen (TestInt a)
arbitraryTestInt b = do
  n <- choose (1, b-1)
  i <- arbitraryInt n
  pure $ TestInt n i

instance Arbitrary (TestInt Int8) where
  arbitrary = arbitraryTestInt 8

instance Arbitrary (TestInt Int16) where
  arbitrary = arbitraryTestInt 16

instance Arbitrary (TestInt Int32) where
  arbitrary = arbitraryTestInt 32

instance Arbitrary (TestInt Int64) where
  arbitrary = arbitraryTestInt 64

instance Binary Word24 where
  get = getWord24be
  put = putWord24be

instance Arbitrary Word24 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink    = shrinkIntegral

decodeBits :: B.BitGet a -> ByteString -> a
decodeBits = G.runGet . B.runBitGet

encodeBits :: B.BitPut () -> ByteString
encodeBits = P.runPut . B.runBitPut

testInt8 :: TestTree
testInt8 =
  testProperty "Roundtrip Int8" $ \(TestInt n i) ->
    decodeBits (getInt8 n) (encodeBits (putInt8 n i)) == (i :: Int8)

testInt16be :: TestTree
testInt16be =
  testProperty "Roundtrip Int16" $ \(TestInt n i) ->
    decodeBits (getInt16be n) (encodeBits (putInt16be n i)) == (i :: Int16)

testInt32be :: TestTree
testInt32be =
  testProperty "Roundtrip Int32" $ \(TestInt n i) ->
    decodeBits (getInt32be n) (encodeBits (putInt32be n i)) == (i :: Int32)

testInt64be :: TestTree
testInt64be =
  testProperty "Roundtrip Int64be" $ \(TestInt n i) ->
    decodeBits (getInt64be n) (encodeBits (putInt64be n i)) == (i :: Int64)

testSInt8 :: TestTree
testSInt8 =
  testProperty "Roundtrip SInt8" $ \(TestInt n i) ->
    decodeBits (getSInt8 n) (encodeBits (putSInt8 n i)) == (i :: Int8)

testSInt16be :: TestTree
testSInt16be =
  testProperty "Roundtrip SInt16" $ \(TestInt n i) ->
    decodeBits (getSInt16be n) (encodeBits (putSInt16be n i)) == (i :: Int16)

testSInt32be :: TestTree
testSInt32be =
  testProperty "Roundtrip SInt32" $ \(TestInt n i) ->
    decodeBits (getSInt32be n) (encodeBits (putSInt32be n i)) == (i :: Int32)

testSInt64be :: TestTree
testSInt64be =
  testProperty "Roundtrip SInt64be" $ \(TestInt n i) ->
    decodeBits (getSInt64be n) (encodeBits (putSInt64be n i)) == (i :: Int64)

testWord24be :: TestTree
testWord24be =
  testProperty "Roundtrip Word24be" $ \i ->
    decode (encode i) === (i :: Word24)

tests :: TestTree
tests =
  testGroup "Extras tests"
    [ testInt8
    , testInt16be
    , testInt32be
    , testInt64be
    , testSInt8
    , testSInt16be
    , testSInt32be
    , testSInt64be
    , testWord24be
    ]
