-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Slack.WebAPI.API
  ( API
  , api
  , SlackResponse(..)
  , User(..)

  ) where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Aeson.Key qualified as Key
import Data.Aeson.TH
import Data.Text (Text)
import Data.Time.Zones.All (TZLabel)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Servant (Get, JSON, Post, Proxy(..), QueryParam', Required, Strict, type (:<|>), type (:>))
import Servant.Auth (Auth, JWT)
import TzBot.Instances ()
import TzBot.Slack.Core.Types (ChannelId, UserId)
import URI.ByteString (URI)
import URI.ByteString.Aeson ()

type RequiredParam = QueryParam' [Required, Strict]

-- | A type describing Slack's Web API: https://api.slack.com/web
type API =
  -- See: https://api.slack.com/methods/apps.connections.open
  Auth '[JWT] Text
    :> "apps.connections.open"
    :> Post '[JSON] (SlackResponse "url" URI)
  :<|>
  -- See: https://api.slack.com/methods/users.info
  Auth '[JWT] Text
    :> "users.info"
    :> RequiredParam "user" UserId
    :> Post '[JSON] (SlackResponse "user" User)
  :<|>
  -- See: https://api.slack.com/methods/conversations.members
  Auth '[JWT] Text
    :> "conversations.members"
    :> RequiredParam "channel" ChannelId
    :> Get '[JSON] (SlackResponse "members" [UserId])
  :<|>
  -- See: https://api.slack.com/methods/chat.postEphemeral
  Auth '[JWT] Text
    :> "chat.postEphemeral"
    :> RequiredParam "user" UserId
    :> RequiredParam "channel" ChannelId
    :> RequiredParam "text" Text
    :> Post '[JSON] (SlackResponse "message_ts" Value)

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
data SlackResponse (key :: Symbol) a
  = SRSuccess a
  | SRError Text
  deriving stock (Show)

instance (KnownSymbol key, FromJSON a) => FromJSON (SlackResponse key a) where
  parseJSON = withObject "SlackResponse" \obj -> do
    obj .: "ok" >>= \case
      True  -> SRSuccess <$> obj .: key
      False -> SRError <$> obj .: "error"
    where
      key = Key.fromString $ symbolVal (Proxy @key)

-- | See: https://api.slack.com/types/user
data User = User
  { uId :: UserId
  , uIsBot :: Bool
  , uTz :: TZLabel
  }
  deriving stock (Eq, Show)
deriveFromJSON (aesonPrefix snakeCase) ''User
