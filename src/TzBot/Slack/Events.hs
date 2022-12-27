-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Slack.Events where

import Universum

import Data.Aeson
import Data.Aeson.Key qualified as AeKey
import Data.Aeson.Types
import Data.Time
import Text.Interpolation.Nyan

import TzBot.Slack.API

data Message = Message
  { mUser :: UserId
  , mText :: Text
  , mMessageId :: MessageId
  , mThreadId :: Maybe ThreadId
  , mEdited :: Bool
  , mSubType :: Maybe Text
  } deriving stock (Eq, Show, Generic)

instance FromJSON Message where
  parseJSON = withObject "Message" parseMessage

parseMessage :: Object -> Parser Message
parseMessage o = do
    mUser <- o .: "user"
    mText <- o .: "text"
    mMessageId <- o .: "ts"
    mThreadId <- o .:? "thread_ts"
    mSubType <- o .:? "subtype"
    -- FIXME: use lenses
    mEdited <- fmap (isJust @Text) . runMaybeT $
      MaybeT (o .:? "edited") >>= MaybeT . (.:? "ts")
    pure Message {..}

data MessageEvent = MessageEvent
  { meChannel :: ChannelId
  , meMessage :: Message
  , meTs :: UTCTime
  , meMessageDetails :: MessageDetails
  } deriving stock (Eq, Show, Generic)

meUser :: MessageEvent -> UserId
meUser = mUser . meMessage

meThreadId :: MessageEvent -> Maybe ThreadId
meThreadId = mThreadId . meMessage

data MessageDetails =
  MDMessage
  | MDMessageEdited Message
  | MDMessageBroadcast -- message copied to channel from thread
  deriving stock (Eq, Show, Generic)

instance FromJSON MessageEvent where
  parseJSON = withObject "" $ \o -> do
    meChannel <- o .: "channel"
    meTs <- fetchSlackTimestamp "ts" o
    (subtype :: Maybe Text) <- o .:? "subtype"

    let parseMessageFromTopObject = (,MDMessage) <$> parseMessage o
    (meMessage, meMessageDetails) <- case subtype of
      Nothing -> parseMessageFromTopObject
      Just "thread_broadcast" -> parseMessageFromTopObject
      Just "message_changed" -> do
        -- Explanation: when someone posts a message to a thread with channel broadcast,
        -- two events come: message and then message_changed, the latter seemingly
        -- corresponds to sending this message directly to the channel.
        -- These messages lack "edited" field, and they are not really edited.
        newMsg <- o .: "message" >>= parseMessage
        case (mEdited newMsg, mSubType newMsg) of
          (True, _) -> do
            prevMsg <- o .: "previous_message" >>= parseMessage
            pure (newMsg, MDMessageEdited prevMsg)
          (False, Just "thread_broadcast") ->
            pure (newMsg, MDMessageBroadcast)
          _ -> fail "expected edited message"
      Just unknownSubtype -> fail $ "unknown subtype " <> toString unknownSubtype
    pure MessageEvent {..}

tsToUTC :: String -> Maybe UTCTime
tsToUTC = parseTimeM False defaultTimeLocale "%s%Q"

parseSlackTimestamp :: AeKey.Key -> String -> Parser UTCTime
parseSlackTimestamp fieldName tsStr = do
  let failMessage = [int||Failed to parse timestamp "#{AeKey.toString fieldName}"|]
  maybe (fail failMessage) pure $ tsToUTC tsStr

fetchSlackTimestamp :: AeKey.Key -> Object -> Parser UTCTime
fetchSlackTimestamp key o = o .: key >>= parseSlackTimestamp key
