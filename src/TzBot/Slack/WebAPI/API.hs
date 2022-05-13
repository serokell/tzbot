module TzBot.Slack.WebAPI.API
  ( API
  , openConnection
  , SlackResponse(..)

  ) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Servant (JSON, Post, Proxy(..), type (:>))
import Servant.Auth as Auth
import Servant.Auth.Client qualified as Auth
import Servant.Client (ClientM, client)
import URI.ByteString
import URI.ByteString.Aeson ()

-- | A type describing Slack's Web API: https://api.slack.com/web
type API =
  Auth '[JWT] Text :>
    -- See: https://api.slack.com/methods/apps.connections.open
    "apps.connections.open" :> Post '[JSON] (SlackResponse "url" URI)

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

----------------------------------------------------------------------------
-- Client
----------------------------------------------------------------------------

api :: Proxy API
api = Proxy

openConnection :: Auth.Token -> ClientM (SlackResponse "url" URI)
openConnection = client api
