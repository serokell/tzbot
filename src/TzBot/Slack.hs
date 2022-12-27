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
  , getUser
  , getChannelMembers
  , sendEphemeralMessage
  ) where

import Universum

import Control.Monad.Except (throwError)
import Data.Aeson (Value)
import Data.Text.Encoding qualified as T
import Servant ((:<|>)(..))
import Servant.Auth.Client qualified as Auth
import Servant.Client
  (BaseUrl(BaseUrl), ClientM, Scheme(Https), client, hoistClient, mkClientEnv, runClientM)

import Data.Text qualified as T
import TzBot.Config
import TzBot.RunMonad
import TzBot.Slack.API

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
sendEphemeralMessage :: ChannelId -> Maybe ThreadId -> Text -> UserId -> BotM ()
sendEphemeralMessage channelId threadId text userId = do
  token <- getBotToken
  void $ postEphemeral token userId channelId threadId text >>= endpointFailed "chat.postEphemeral"

getBotToken :: BotM Auth.Token
getBotToken = do
  BotToken bt <- asks $ bcBotToken . bsConfig
  pure $ Auth.Token $ T.encodeUtf8 bt

endpointFailed :: Text -> SlackResponse key a -> BotM a
endpointFailed endpoint = \case
  SRSuccess a -> pure a
  SRError err -> do
    log' $ endpoint <> " error: " <> T.pack (show err)
    throwError $ EndpointFailed endpoint err

----------------------------------------------------------------------------
-- Endpoints
----------------------------------------------------------------------------

usersInfo :: Auth.Token -> UserId -> BotM (SlackResponse "user" User)
conversationMembers
  :: Auth.Token -> ChannelId -> Limit
  -> BotM (SlackResponse "members" [UserId])
postEphemeral
  :: Auth.Token -> UserId -> ChannelId -> Maybe ThreadId -> Text
  -> BotM (SlackResponse "message_ts" Value)

usersInfo
  :<|> conversationMembers
  :<|> postEphemeral =
  hoistClient api naturalTransformation (client api)
  where
    baseUrl = BaseUrl Https "slack.com" 443 "api"

    naturalTransformation :: ClientM a -> BotM a
    naturalTransformation act = BotM do
      manager <- asks bsManager
      let clientEnv = mkClientEnv manager baseUrl
      liftIO (runClientM act clientEnv) >>= \case
        Right a -> pure a
        Left clientError -> throwError $ ServantError clientError
