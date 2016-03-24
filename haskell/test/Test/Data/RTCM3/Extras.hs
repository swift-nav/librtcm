{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.Data.RTCM3.Extras
  ( tests
  ) where

import           BasicPrelude
import           Data.Binary
import           Data.Binary.Bits
import qualified Data.Binary.Bits.Get as B
import qualified Data.Binary.Bits.Put as B
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString.Lazy as B
import           Data.Int
import           Data.RTCM3.Internal
import           Data.Word.Word24
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           System.Random

instance Binary Word24 where
  get = getWord24be
  put = putWord24be

instance Arbitrary Word24 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink    = shrinkIntegral

instance BinaryBit Int64 where
  getBits = getInt64be
  putBits = putInt64be

data TestInt a = TestInt Int a deriving ( Show, Read, Eq )

arbitraryInt :: (Integral a, Bits a, Random a) => Int -> Gen (TestInt a)
arbitraryInt b = do
  n <- choose (1, b-1)
  let m = (1 `shiftL` (n - 1)) - 1
  i <- choose (-m, m)
  return $ TestInt n i

instance Arbitrary (TestInt Int8) where
  arbitrary = arbitraryInt 8

instance Arbitrary (TestInt Int16) where
  arbitrary = arbitraryInt 16

instance Arbitrary (TestInt Int32) where
  arbitrary = arbitraryInt 32

instance Arbitrary (TestInt Int64) where
  arbitrary = arbitraryInt 64

decodeBits :: B.BitGet a -> B.ByteString -> a
decodeBits = runGet . B.runBitGet

encodeBits :: B.BitPut () -> B.ByteString
encodeBits = runPut . B.runBitPut

testInt8 :: TestTree
testInt8 =
  testProperty "Roundtrip Int8" $ \(TestInt n i) ->
    (decodeBits (getInt8 n) $ encodeBits (putInt8 n i)) == (i :: Int8)

testInt16be :: TestTree
testInt16be =
  testProperty "Roundtrip Int16" $ \(TestInt n i) ->
    (decodeBits (getInt16be n) $ encodeBits (putInt16be n i)) == (i :: Int16)

testInt32be :: TestTree
testInt32be =
  testProperty "Roundtrip Int32" $ \(TestInt n i) ->
    (decodeBits (getInt32be n) $ encodeBits (putInt32be n i)) == (i :: Int32)

testInt64be :: TestTree
testInt64be =
  testProperty "Roundtrip Int64be" $ \(TestInt n i) ->
    (decodeBits (getInt64be n) $ encodeBits (putInt64be n i)) == (i :: Int64)

testWord24be :: TestTree
testWord24be =
  testProperty "Roundtrip Word24be" $ \i ->
    (decode $ encode i) === (i :: Word24)

tests :: TestTree
tests =
  testGroup "Extras tests"
    [ testInt8
    , testInt16be
    , testInt32be
    , testInt64be
    , testWord24be
    ]
