module TzBot.Slack.WebAPI.Class where

import Data.Text (Text)
import TzBot.Slack.Core.Types (ChannelId, UserId)
import TzBot.Slack.WebAPI.API (User)
import URI.ByteString (URI)

class Monad m => WebAPI m where
  -- | Generate a temporary Socket Mode WebSocket URL to connect to and receive events.
  -- openConnection :: m OpenConnectionResponse
  genWebSocketsURI :: m URI

  -- | Get a user's info.
  getUser :: UserId -> m User

  -- | Get a list of a channel's members.
  getChannelMembers :: ChannelId -> m [UserId]

  -- | Post an "ephemeral message", a message only visible to the given user.
  sendEphemeralMessage :: UserId -> ChannelId -> Text -> m ()
