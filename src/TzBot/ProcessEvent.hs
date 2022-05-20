module TzBot.ProcessEvent where

import Data.Text (Text)
import Data.Time (UTCTime)
import TzBot.Slack.Core.Types (ChannelId, UserId)
import TzBot.Slack.WebAPI.Class (WebAPI)

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
  }

processEvent :: WebAPI m => EventSummary -> m ()
processEvent _evt = do
  -- TODO [#4]
  pure ()
