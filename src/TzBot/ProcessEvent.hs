-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvent where

import Control.Lens ((^?!), (^?))
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError)
import TzBot.Slack.Core.Types (ChannelId(..), UserId(..))
import TzBot.Slack

data EventSummary = EventSummary
  { esMessage :: Text
  -- ^ The message that was sent to a Slack channel
  , esPreviousMessage :: Maybe Text
  -- ^ When a message is edited, the event will contain the previous message text.
  , esSender :: UserId
  -- ^ The ID of the user that sent this message to a Slack channel.
  , esChannel :: ChannelId
  -- ^ The ID of the channel the message was sent to.
  , esTimestamp :: UTCTime
  -- ^ The time at which the message was sent.
  } deriving stock Show

extractUserId :: Data.Aeson.Value -> UserId
extractUserId val = UserId $ val ^?! key "user" . _String

extractChannelId :: Data.Aeson.Value -> ChannelId
extractChannelId val = ChannelId $ val ^?! key "channel" . _String

extractTimeStamp :: Data.Aeson.Value -> UTCTime
extractTimeStamp val = tsToUTC . unpack $ val ^?! key "ts" . _String
  where tsToUTC = parseTimeOrError False defaultTimeLocale "%s%Q"

constructEventSummary :: Data.Aeson.Value -> EventSummary
constructEventSummary evt = EventSummary message previousMessage uid (extractChannelId evt) (extractTimeStamp evt)
  where previousMessageObj = evt ^? key "previous_message" . _Value
        previousMessage = evt ^? key "previous_message" . key "text" . _String

        message = fromJust $ case previousMessage of
          Nothing -> evt ^? key "text" . _String
          Just _  -> evt ^? key "message" . key "text" . _String

        -- We always extract the user id of the original message in case of forced edits
        uid = case previousMessageObj of
          Nothing  -> extractUserId evt
          Just obj -> extractUserId obj

processEvent :: EventSummary -> WebAPIM ()
processEvent _evt = do
  -- TODO [#4]
  pure ()
