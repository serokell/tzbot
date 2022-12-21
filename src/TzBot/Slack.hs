-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Slack
  ( BotM(..)
  , runBotM
  , runOrThrowBotM
  , AppLevelToken(..)
  , BotToken(..)
  , BotState(..)
  , BotConfig(..)
  , BotException(..)
  , genWebSocketsURI
  , getUser
  , getChannelMembers
  , sendEphemeralMessage
  ) where

import Control.Monad (void)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (Value)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Servant ((:<|>)(..))
import Servant.Auth.Client qualified as Auth
import Servant.Client
  (BaseUrl(BaseUrl), ClientM, Scheme(Https), client, hoistClient, mkClientEnv, runClientM)
import URI.ByteString (URI)

import TzBot.RunMonad
import TzBot.Slack.API

-- | Generate a temporary Socket Mode WebSocket URL to connect to and receive events.
genWebSocketsURI :: BotM URI
genWebSocketsURI = do
  token <- getAppLevelToken
  openConnection token >>= endpointFailed "apps.connections.open"

-- | Get a user's info.
getUser :: UserId -> BotM User
getUser userId = do
  token <- getBotToken
  usersInfo token userId >>= endpointFailed "users.info"

-- | Get a list of a channel's members.
getChannelMembers :: ChannelId -> BotM [UserId]
getChannelMembers channelId = do
  token <- getBotToken
  let limit = Limit {limitQ = 200}
  conversationMembers token channelId limit >>= endpointFailed "conversations.members"

-- | Post an "ephemeral message", a message only visible to the given user.
sendEphemeralMessage :: UserId -> ChannelId -> Text -> BotM ()
sendEphemeralMessage userId channelId text = do
  token <- getBotToken
  void $ postEphemeral token userId channelId text >>= endpointFailed "chat.postEphemeral"

getAppLevelToken :: BotM Auth.Token
getAppLevelToken = do
  AppLevelToken alt <- asks $ wacAppLevelToken . wasConfig
  pure $ Auth.Token $ T.encodeUtf8 alt

getBotToken :: BotM Auth.Token
getBotToken = do
  BotToken bt <- asks $ wacBotToken . wasConfig
  pure $ Auth.Token $ T.encodeUtf8 bt

endpointFailed :: Text -> SlackResponse key a -> BotM a
endpointFailed endpoint = \case
  SRSuccess a -> pure a
  SRError err -> throwError $ EndpointFailed endpoint err

----------------------------------------------------------------------------
-- Endpoints
----------------------------------------------------------------------------

openConnection :: Auth.Token -> BotM (SlackResponse "url" URI)
usersInfo :: Auth.Token -> UserId -> BotM (SlackResponse "user" User)
conversationMembers
  :: Auth.Token -> ChannelId -> Limit
  -> BotM (SlackResponse "members" [UserId])
postEphemeral
  :: Auth.Token -> UserId -> ChannelId -> Text
  -> BotM (SlackResponse "message_ts" Value)

openConnection
  :<|> usersInfo
  :<|> conversationMembers
  :<|> postEphemeral =
  hoistClient api naturalTransformation (client api)
  where
    baseUrl = BaseUrl Https "slack.com" 443 "api"

    naturalTransformation :: ClientM a -> BotM a
    naturalTransformation act = BotM do
      manager <- asks wasManager
      let clientEnv = mkClientEnv manager baseUrl
      liftIO (runClientM act clientEnv) >>= \case
        Right a -> pure a
        Left clientError -> throwError $ ServantError clientError
