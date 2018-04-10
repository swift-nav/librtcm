{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module:      Data.RTCM3.System
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- RTCMv3 System.

module Data.RTCM3.System
  ( module Data.RTCM3.System
  ) where

import           BasicPrelude
import           Control.Lens
import           Data.Aeson.TH
import           Data.Binary
import           Data.Binary.Bits
import qualified Data.Binary.Bits.Get as B
import qualified Data.Binary.Bits.Put as B
import           Data.RTCM3.TH

{-# ANN module ("HLint: ignore Use camelCase"::String) #-}

-- | MessageHeader
--
-- Messages and system parameters header.
data MessageHeader = MessageHeader
  { _messageHeader_num         :: Word16
    -- ^ Message number.
  , _messageHeader_station     :: Word16
    -- ^ Reference station id.
  , _messageHeader_mjd         :: Word16
    -- ^ Modified Julian Day Number.
  , _messageHeader_seconds     :: Word32
    -- ^ Seconds of Day.
  , _messageHeader_n           :: Word8
    -- ^ Number of messages.
  , _messageHeader_leapSeconds :: Word8
    -- ^ Leap Seconds.
  } deriving ( Show, Read, Eq )

$(makeLenses ''MessageHeader)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_messageHeader_" . stripPrefix "_messageHeader_"} ''MessageHeader)

instance BinaryBit MessageHeader where
  getBits _n = do
    _messageHeader_num         <- B.getWord16be 12
    _messageHeader_station     <- B.getWord16be 12
    _messageHeader_mjd         <- B.getWord16be 16
    _messageHeader_seconds     <- B.getWord32be 17
    _messageHeader_n           <- B.getWord8 5
    _messageHeader_leapSeconds <- B.getWord8 8
    pure MessageHeader {..}

  putBits _n MessageHeader {..} = do
    B.putWord16be 12 _messageHeader_num
    B.putWord16be 12 _messageHeader_station
    B.putWord16be 16 _messageHeader_mjd
    B.putWord32be 17 _messageHeader_seconds
    B.putWord8 5     _messageHeader_n
    B.putWord8 8     _messageHeader_leapSeconds

-- | Message
--
-- Transmitted message.
data Message = Message
  { _message_num         :: Word16
    -- ^ Message number.
  , _message_synchronous :: Bool
    -- ^ Synchronous flag.
  , _message_interval    :: Word16
    -- ^ Transmission interval.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Message)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_message_" . stripPrefix "_message_"} ''Message)

instance BinaryBit Message where
  getBits _n = do
    _message_num         <- B.getWord16be 12
    _message_synchronous <- B.getBool
    _message_interval    <- B.getWord16be 16
    pure Message {..}

  putBits _n Message {..} = do
    B.putWord16be 12 _message_num
    B.putBool        _message_synchronous
    B.putWord16be 16 _message_interval

-- | TextMessage
--
-- Informational text message.
data TextMessage = TextMessage
  { _textMessage_num         :: Word16
    -- ^ Message number.
  , _textMessage_station     :: Word16
    -- ^ Reference station id.
  , _textMessage_mjd         :: Word16
    -- ^ Modified Julian Day Number.
  , _textMessage_seconds     :: Word32
    -- ^ Seconds of Day.
  , _textMessage_characters  :: Word8
    -- ^ Number of characters.
  , _textMessage_n           :: Word8
    -- ^ Number of UTF-8 code units.
  , _textMessage_text        :: Text
    -- ^ UTF-8 code units.
  } deriving ( Show, Read, Eq )

$(makeLenses ''TextMessage)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_textMessage_" . stripPrefix "_textMessage_"} ''TextMessage)

instance BinaryBit TextMessage where
  getBits _n = do
    _textMessage_num        <- B.getWord16be 12
    _textMessage_station    <- B.getWord16be 12
    _textMessage_mjd        <- B.getWord16be 16
    _textMessage_seconds    <- B.getWord32be 17
    _textMessage_characters <- B.getWord8 7
    _textMessage_n          <- B.getWord8 8
    _textMessage_text       <- decodeUtf8 <$> B.getByteString (fromIntegral _textMessage_n)
    pure TextMessage {..}

  putBits _n TextMessage {..} = do
    B.putWord16be 12 _textMessage_num
    B.putWord16be 12 _textMessage_station
    B.putWord16be 16 _textMessage_mjd
    B.putWord32be 17 _textMessage_seconds
    B.putWord8 7     _textMessage_characters
    B.putWord8 8     _textMessage_n
    B.putByteString $ encodeUtf8 _textMessage_text

msg1013 :: Word16
msg1013 = 1013

-- | Msg1013.
--
-- RTCMv3 message 1013.
data Msg1013 = Msg1013
  { _msg1013_header   :: MessageHeader
    -- ^ Messages header.
  , _msg1013_messages :: [Message]
    -- ^ Messages.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1013)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1013_" . stripPrefix "_msg1013_"} ''Msg1013)

instance Binary Msg1013 where
  get = B.runBitGet $ do
    _msg1013_header   <- getBits 0
    _msg1013_messages <- replicateM (fromIntegral $ _msg1013_header ^. messageHeader_n) $ getBits 0
    pure Msg1013 {..}

  put Msg1013 {..} = B.runBitPut $ do
    putBits 0 _msg1013_header
    forM_ _msg1013_messages $ putBits 0

$(deriveRTCM3 ''Msg1013)

msg1029 :: Word16
msg1029 = 1029

-- | Msg1029.
--
-- RTCMv3 message 1029.
newtype Msg1029 = Msg1029
  { _msg1029_message :: TextMessage
    -- ^ Text Messages.
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1029)
$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msg1029_" . stripPrefix "_msg1029_"} ''Msg1029)

instance Binary Msg1029 where
  get = B.runBitGet $ do
    _msg1029_message <- getBits 0
    pure Msg1029 {..}

  put Msg1029 {..} = B.runBitPut $
    putBits 0 _msg1029_message

$(deriveRTCM3 ''Msg1029)

