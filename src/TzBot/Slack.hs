-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Slack
  ( WebAPIM(..)
  , runWebAPIM
  , runOrThrowWebAPIM
  , AppLevelToken(..)
  , BotToken(..)
  , WebAPIState(..)
  , WebAPIConfig(..)
  , WebAPIException(..)
  , genWebSocketsURI
  , getUser
  , getChannelMembers
  , sendEphemeralMessage
  ) where

import Control.Exception.Safe (Exception(..), throwM)
import Control.Monad (void)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Data.Aeson (Value)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Network.HTTP.Client (Manager)
import Servant ((:<|>)(..))
import Servant.Auth.Client qualified as Auth
import Servant.Client
  (BaseUrl(BaseUrl), ClientError, ClientM, Scheme(Https), client, hoistClient, mkClientEnv,
  runClientM)
import Text.Interpolation.Nyan
import TzBot.Slack.API
import URI.ByteString (URI)

newtype AppLevelToken = AppLevelToken { unAppLevelToken :: Text }
newtype BotToken = BotToken { unBotToken :: Text }

data WebAPIConfig = WebAPIConfig
  { wacAppLevelToken :: AppLevelToken
  , wacBotToken :: BotToken
  }

data WebAPIState = WebAPIState
  { wasConfig :: WebAPIConfig
  , wasManager :: Manager
  }

-- | An implementation of `WebApi`.
newtype WebAPIM a = WebAPIM
  { unWebAPIM :: ReaderT WebAPIState (ExceptT WebAPIException IO) a
  }
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader WebAPIState, MonadError WebAPIException
    )

runWebAPIM :: WebAPIState -> WebAPIM a -> IO (Either WebAPIException a)
runWebAPIM state webApiM =
  webApiM
    & unWebAPIM
    & flip runReaderT state
    & runExceptT

runOrThrowWebAPIM :: WebAPIState -> WebAPIM a -> IO a
runOrThrowWebAPIM state webApiM =
  runWebAPIM state webApiM >>= either throwM pure

-- | Generate a temporary Socket Mode WebSocket URL to connect to and receive events.
genWebSocketsURI :: WebAPIM URI
genWebSocketsURI = do
  token <- getAppLevelToken
  openConnection token >>= endpointFailed "apps.connections.open"

-- | Get a user's info.
getUser :: UserId -> WebAPIM User
getUser userId = do
  token <- getBotToken
  usersInfo token userId >>= endpointFailed "users.info"

-- | Get a list of a channel's members.
getChannelMembers :: ChannelId -> WebAPIM [UserId]
getChannelMembers channelId = do
  token <- getBotToken
  let limit = Limit {limitQ = 200}
  conversationMembers token channelId limit >>= endpointFailed "conversations.members"

-- | Post an "ephemeral message", a message only visible to the given user.
sendEphemeralMessage :: UserId -> ChannelId -> Text -> WebAPIM ()
sendEphemeralMessage userId channelId text = do
  token <- getBotToken
  void $ postEphemeral token userId channelId text >>= endpointFailed "chat.postEphemeral"

getAppLevelToken :: WebAPIM Auth.Token
getAppLevelToken = do
  AppLevelToken alt <- asks $ wacAppLevelToken . wasConfig
  pure $ Auth.Token $ T.encodeUtf8 alt

getBotToken :: WebAPIM Auth.Token
getBotToken = do
  BotToken bt <- asks $ wacBotToken . wasConfig
  pure $ Auth.Token $ T.encodeUtf8 bt

endpointFailed :: Text -> SlackResponse key a -> WebAPIM a
endpointFailed endpoint = \case
  SRSuccess a -> pure a
  SRError err -> throwError $ EndpointFailed endpoint err

----------------------------------------------------------------------------
-- Endpoints
----------------------------------------------------------------------------

openConnection :: Auth.Token -> WebAPIM (SlackResponse "url" URI)
usersInfo :: Auth.Token -> UserId -> WebAPIM (SlackResponse "user" User)
conversationMembers
  :: Auth.Token -> ChannelId -> Limit
  -> WebAPIM (SlackResponse "members" [UserId])
postEphemeral
  :: Auth.Token -> UserId -> ChannelId -> Text
  -> WebAPIM (SlackResponse "message_ts" Value)

openConnection
  :<|> usersInfo
  :<|> conversationMembers
  :<|> postEphemeral =
  hoistClient api naturalTransformation (client api)
  where
    baseUrl = BaseUrl Https "slack.com" 443 "api"

    naturalTransformation :: ClientM a -> WebAPIM a
    naturalTransformation act = WebAPIM do
      manager <- asks wasManager
      let clientEnv = mkClientEnv manager baseUrl
      liftIO (runClientM act clientEnv) >>= \case
        Right a -> pure a
        Left clientError -> throwError $ ServantError clientError

----------------------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------------------

data WebAPIException
  = EndpointFailed Text Text
  | ServantError ClientError
  deriving stock Show

instance Exception WebAPIException where
  displayException = \case
    EndpointFailed endpoint err ->
      [int|s|
        '#{endpoint}' failed:
          #{err}
      |]
    ServantError clientError ->
      displayException clientError
