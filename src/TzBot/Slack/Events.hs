-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Slack.Events where

import TzPrelude

import Data.Aeson (FromJSON(parseJSON), ToJSON, withObject, withText, (.:), (.:?))
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Text.Interpolation.Nyan (int, rmode')

import TzBot.Feedback.Dialog.Types (ReportDialogId)
import TzBot.Slack.API
import TzBot.Slack.Events.ViewPayload
import TzBot.Slack.Fixtures qualified as Fixtures
import TzBot.Util (RecordWrapper(..), fetchSlackTimestamp)

data MessageEvent = MessageEvent
  { meChannel :: ChannelId
  , meChannelType :: Maybe ChannelType
  , meMessage :: Message
  , meTs :: UTCTime
  , meMessageDetails :: MessageDetails
  } deriving stock (Eq, Show, Generic)

meUser :: MessageEvent -> UserId
meUser = mUser . meMessage

meThreadId :: MessageEvent -> Maybe ThreadId
meThreadId = mThreadId . meMessage

data MessageDetails
  = MDMessage
  | MDMessageEdited Message
  | MDMessageBroadcast -- message copied to channel from thread
  | MDMessageUrlUnfurl
    -- ^ This event is occurs when a user posts a message with a URL
    -- and Slack displays a URL preview.
  | MDUserJoinedChannel
  | MDUserLeftChannel
  deriving stock (Eq, Show, Generic)

instance FromJSON MessageEvent where
  parseJSON = withObject "MessageEvent" $ \o -> do
    meChannel <- o .: "channel"
    meChannelType <- o .:? "channel_type"
    meTs <- fetchSlackTimestamp "ts" o
    (subtype :: Maybe Text) <- o .:? "subtype"

    let parseMessageFromTopObject = (,MDMessage) <$> parseMessage o
    (meMessage, meMessageDetails) <- case subtype of
      Nothing -> parseMessageFromTopObject
      Just "thread_broadcast" -> parseMessageFromTopObject
      Just "channel_join"     -> (,MDUserJoinedChannel) <$> parseMessage o
      Just "channel_leave"    -> (,MDUserLeftChannel) <$> parseMessage o
      Just "message_changed"  -> do
        newMsg <- o .: "message" >>= parseMessage

        messageDetails <-
          if
            | newMsg.mEdited -> do
                prevMsg <- o .: "previous_message" >>= parseMessage
                pure $ MDMessageEdited prevMsg
            | not newMsg.mEdited && newMsg.mSubType == Just "thread_broadcast" ->
                -- Explanation: when someone posts a message to a thread with channel broadcast,
                -- two events come: message and then message_changed, the latter seemingly
                -- corresponds to sending this message directly to the channel.
                -- These messages lack "edited" field, and they are not really edited.
                pure MDMessageBroadcast
            | not newMsg.mEdited && subtype == Just "message_changed" ->
                pure MDMessageUrlUnfurl
            | otherwise -> fail "expected edited message"

        pure (newMsg, messageDetails)
      Just _unknownSubtype -> parseMessageFromTopObject
    pure MessageEvent {..}

-- | See https://api.slack.com/events/member_left_channel
data MemberLeftChannelEvent = MemberLeftChannelEvent
  { mlceChannel :: ChannelId
  , mlceChannelType :: Text
  , mlceUser :: UserId
  } deriving stock (Eq, Show, Generic)
    deriving (FromJSON, ToJSON) via RecordWrapper MemberLeftChannelEvent

-- | See https://api.slack.com/events/member_joined_channel
data MemberJoinedChannelEvent = MemberJoinedChannelEvent
  { mjceChannel :: ChannelId
  , mjceChannelType :: Text
  , mjceUser :: UserId
  , mjceInviter :: Maybe UserId
  } deriving stock (Eq, Show, Generic)
    deriving (FromJSON, ToJSON) via RecordWrapper MemberJoinedChannelEvent

-- time related utils
tsToUTC :: String -> Maybe UTCTime
tsToUTC = parseTimeM False defaultTimeLocale "%s%Q"
--
data CallbackType = CTView | CTReport
  deriving stock (Eq, Show, Read, Generic)

instance FromJSON CallbackType where
  parseJSON = withText "CallbackType" $ \case
    Fixtures.ViewEntrypointCallbackId   -> pure CTView
    Fixtures.ReportEntrypointCallbackId -> pure CTReport
    e -> fail [int||Unknown callback type #{e}|]

-- | See https://api.slack.com/reference/interaction-payloads/shortcuts
data InteractiveMessageEvent = InteractiveMessageEvent
  { imeCallbackId :: CallbackType
  , imeMessage :: Message
  , imeUser :: ShortUser
  , imeChannel :: ShortChannel
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

-- | See https://api.slack.com/reference/interaction-payloads/views
data ViewActionEvent a = ViewActionEvent
  { vaeView :: View a
  } deriving stock (Eq, Show, Generic)
    deriving FromJSON via RecordWrapper (ViewActionEvent a)

data UserFeedbackPayload = UserFeedbackPayload
  { ufpUserInput ::
      ViewPayload
        Fixtures.ReportInputBlockId
        Fixtures.ReportInputElementActionId
        Text
  } deriving stock (Show, Generic)
    deriving FromJSON via PayloadCollection UserFeedbackPayload

type SubmitViewEvent = ViewActionEvent UserFeedbackPayload

-- | See https://api.slack.com/reference/interaction-payloads/views
data View a = View
  { vId :: ViewId
  , vRootViewId :: ViewId
  , vCallbackId :: Text
  , vPrivateMetadata :: ReportDialogId
  , vState :: a
  } deriving stock (Eq, Show, Generic)
    deriving FromJSON via RecordWrapper (View a)
