module TzBot.Slack.WebAPI.Class where

import URI.ByteString (URI)

class Monad m => WebAPI m where
  -- | Generate a temporary Socket Mode WebSocket URL to connect to and receive events.
  -- openConnection :: m OpenConnectionResponse
  genWebSocketsURI :: m URI
