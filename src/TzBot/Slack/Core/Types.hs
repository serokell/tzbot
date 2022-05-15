
-- | Types shared across multiple Slack APIs (e.g. Web API, Events API, or socket mode).
module TzBot.Slack.Core.Types where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import Servant (ToHttpApiData)

newtype UserId = UserId { unUserId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToHttpApiData)

newtype ChannelId = ChannelId { unChannelId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (ToHttpApiData)
