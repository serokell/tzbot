-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Slack.Events where

import Universum

import Data.Aeson
import Data.Time
import Text.Interpolation.Nyan

import TzBot.Feedback.Dialog.Types
import TzBot.Slack.API
import TzBot.Slack.Fixtures qualified as Fixtures
import TzBot.Util

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

--
data CallbackType = CTView | CTReport
  deriving stock (Eq, Show, Read, Generic)

instance FromJSON CallbackType where
  parseJSON = withText "CallbackType" $ \case
    e | e == Fixtures.viewEntrypointCallbackId -> pure CTView
      | e == Fixtures.reportEntrypointCallbackId -> pure CTReport
      | otherwise -> fail [int||unknown callback type #{e}|]

{-
>>> parseEither parseJSON "tz_report" :: Either String CallbackType
Right CTReport
 -}

-- | See https://api.slack.com/reference/interaction-payloads/shortcuts
data InteractiveMessageEvent = InteractiveMessageEvent
  { imeCallbackId :: CallbackType
  , imeMessage :: Message
  , imeUser :: ShortUser
  , imeTriggerId :: TriggerId
  } deriving stock (Eq, Show, Generic)
    deriving FromJSON via RecordWrapper InteractiveMessageEvent

data ShortUser = ShortUser
  { suId :: UserId
  , suName :: Text
  } deriving stock (Eq, Show, Generic)
    deriving FromJSON via RecordWrapper ShortUser

data ShortChannel = ShortChannel
  { scId :: ChannelId
  , scName :: Text
  } deriving stock (Eq, Show, Generic)
    deriving FromJSON via RecordWrapper ShortChannel

data ReportEphemeralEvent = ReportEphemeralEvent
  { reeTriggerId :: TriggerId
  , reeUser :: ShortUser
  , reeChannel :: ShortChannel
  } deriving stock (Eq, Show, Generic)
    deriving FromJSON via RecordWrapper ReportEphemeralEvent

-- | See
data ViewActionEvent = ViewActionEvent
  { vaeView :: View
  } deriving stock (Eq, Show, Generic)
    deriving FromJSON via RecordWrapper ViewActionEvent

-- | See https://api.slack.com/reference/interaction-payloads/views
data View = View
  { vId :: ViewId
  , vRootViewId :: ViewId
  , vCallbackId :: Text
  , vPrivateMetadata :: ReportDialogId
  , vState :: Value
  } deriving stock (Eq, Show, Generic)
    deriving FromJSON via RecordWrapper View
