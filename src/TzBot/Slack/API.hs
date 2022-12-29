-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Slack.API
  ( API
  , api
  , SlackResponse(..)
  , SlackContents(..)
  , User(..)
  , UserId(..)
  , ChannelId(..)
  , Cursor(..)
  , ThreadId(..)
  , MessageId(..)
  , Limit(..)
  ) where

import Universum

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Aeson.Key qualified as Key
import Data.Aeson.TH
import Data.Time.Zones.All (TZLabel)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Servant
  (Get, JSON, Post, QueryParam, QueryParam', Required, Strict, ToHttpApiData, type (:<|>),
  type (:>))
import Servant.Auth (Auth, JWT)
import TzBot.Instances ()

----------------------------------------------------------------------------
-- API
----------------------------------------------------------------------------

type RequiredParam = QueryParam' [Required, Strict]

-- | A type describing Slack's Web API: https://api.slack.com/web
type API =
  -- See: https://api.slack.com/methods/users.info
  Auth '[JWT] Text
    :> "users.info"
    :> RequiredParam "user" UserId
    :> Post '[JSON] (SlackResponse $ SlackContents "user" User)
  :<|>
  -- See: https://api.slack.com/methods/conversations.members
  Auth '[JWT] Text
    :> "conversations.members"
    :> RequiredParam "channel" ChannelId
    :> RequiredParam "limit" Limit
    :> QueryParam "cursor" Cursor
    :> Get '[JSON] (SlackResponse $ SlackContents "members" $ [UserId])
  :<|>
  -- See: https://api.slack.com/methods/chat.postEphemeral
  Auth '[JWT] Text
    :> "chat.postEphemeral"
    :> RequiredParam "user" UserId
    :> RequiredParam "channel" ChannelId
    :> QueryParam "thread_ts" ThreadId
    :> RequiredParam "text" Text
    :> Post '[JSON] (SlackResponse $ SlackContents "message_ts" Value)

api :: Proxy API
api = Proxy

{- | Slack's responses are always wrapped in a json object with one of these two formats:

> {
>   "ok": true,
>   "some-key": { /* response */ }
> }

> {
>   "ok": false,
>   "error": "error_code"
> }

This data type helps abstract over that pattern.
-}
data SlackResponse a
  = SRSuccess a
  | SRError Text
  deriving stock (Show)

instance (FromJSON a) => FromJSON (SlackResponse a) where
  parseJSON val = ($ val) $ withObject "SlackResponse" \obj -> do
    obj .: "ok" >>= \case
      True  -> SRSuccess <$> parseJSON val
      False -> SRError <$> obj .: "error"

data SlackContents (key :: Symbol) a = SlackContents
  { scContents :: a
  , scCursor :: Maybe Cursor
  } deriving stock (Show)

instance (KnownSymbol key, FromJSON a) => FromJSON (SlackContents key a) where
  parseJSON = withObject "SlackContents" \obj -> do
    scCursor <- runMaybeT $ do
      cursor <- MaybeT (obj .:? "response_metadata") >>= MaybeT . (.:? "next_cursor")
      guard $ not $ null $ unCursor cursor
      pure cursor
    let key = Key.fromString $ symbolVal (Proxy @key)
    scContents <- obj .: key
    pure SlackContents {..}

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

newtype UserId = UserId { unUserId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (ToHttpApiData, FromJSON)

newtype ChannelId = ChannelId { unChannelId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (ToHttpApiData, FromJSON)

newtype ThreadId = ThreadId { unThreadId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (ToHttpApiData, FromJSON)

newtype MessageId = MessageId { unMessageId :: Text }
  deriving stock (Eq, Show, Ord)
  deriving newtype (ToHttpApiData, FromJSON)

newtype Limit = Limit { limitQ :: Int}
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToHttpApiData)

newtype Cursor = Cursor { unCursor :: Text }
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToHttpApiData)

-- | See: https://api.slack.com/types/user
data User = User
  { uId :: UserId
  , uIsBot :: Bool
  , uTz :: TZLabel
  }
  deriving stock (Eq, Show)
deriveFromJSON (aesonPrefix snakeCase) ''User
