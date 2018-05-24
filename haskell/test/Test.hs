{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module:      Test
-- Copyright:   (c) 2015 Swift Navigation
-- License:     BSD3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
--
-- Test module for RTCM3.

import           BasicPrelude
import qualified Test.Data.CRC24Q             as CRC24Q
import qualified Test.Data.RTCM3.Antennas     as Antennas
import qualified Test.Data.RTCM3.Ephemerides  as Ephemerides
import qualified Test.Data.RTCM3.Extras       as Extras
import qualified Test.Data.RTCM3.MSM          as MSM
import qualified Test.Data.RTCM3.Observations as Observations
import qualified Test.Data.RTCM3.SSR          as SSR
import qualified Test.Data.RTCM3.System       as System
import           Test.Tasty

tests :: TestTree
tests =
  testGroup "Tests"
    [ CRC24Q.tests
    , Antennas.tests
    , Ephemerides.tests
    , Extras.tests
    , MSM.tests
    , Observations.tests
    , SSR.tests
    , System.tests
    ]

main :: IO ()
main = defaultMain tests
