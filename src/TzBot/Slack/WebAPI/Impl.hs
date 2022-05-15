module TzBot.Slack.WebAPI.Impl
  ( WebAPIM(..)
  , runWebAPIM
  , runOrThrowWebAPIM
  , AppLevelToken(..)
  , BotToken(..)
  , WebAPIConfig(..)
  , WebAPIException(..)
  ) where

import Control.Exception.Safe (Exception(..), throwM)
import Control.Monad (void)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Reader (MonadReader, MonadTrans(lift), ReaderT, asks, runReaderT)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Servant.Auth.Client qualified as Auth
import Servant.Client (ClientEnv, ClientError, ClientM, runClientM)
import Text.Interpolation.Nyan
import TzBot.Slack.WebAPI.API (SlackResponse(..))
import TzBot.Slack.WebAPI.API qualified as API
import TzBot.Slack.WebAPI.Class (WebAPI(..))

newtype AppLevelToken = AppLevelToken { unAppLevelToken :: Text }
newtype BotToken = BotToken { unBotToken :: Text }

data WebAPIConfig = WebAPIConfig
  { wacAppLevelToken :: AppLevelToken
  , wacBotToken :: BotToken
  }

-- | An implementation of `WebApi`.
newtype WebAPIM a = WebAPIM
  { unWebAPIM :: ReaderT WebAPIConfig (ExceptT WebAPIException ClientM) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadReader WebAPIConfig)

runWebAPIM
  :: WebAPIConfig -> ClientEnv -> WebAPIM a
  -> IO (Either ClientError (Either WebAPIException a))
runWebAPIM config env webApiM =
  webApiM
    & unWebAPIM
    & flip runReaderT config
    & runExceptT
    & flip runClientM env

runOrThrowWebAPIM
  :: WebAPIConfig -> ClientEnv -> WebAPIM a
  -> IO a
runOrThrowWebAPIM config env webApiM =
  runWebAPIM config env webApiM >>= \case
    Left clientError -> throwM clientError
    Right (Left webApiEx) -> throwM webApiEx
    Right (Right a) -> pure a

instance WebAPI WebAPIM where
  genWebSocketsURI = WebAPIM do
    token <- getAppLevelToken
    lift $ lift (API.openConnection token) >>= endpointFailed "apps.connections.open"
  getUser userId = WebAPIM do
    token <- getBotToken
    lift $ lift (API.usersInfo token userId) >>= endpointFailed "users.info"
  getChannelMembers channelId = WebAPIM do
    token <- getBotToken
    lift $ lift (API.conversationMembers token channelId) >>= endpointFailed "conversations.members"
  postEphemeral userId channelId text = WebAPIM do
    token <- getBotToken
    void $ lift $ lift (API.postEphemeral token userId channelId text)
      >>= endpointFailed "chat.postEphemeral"

getAppLevelToken :: MonadReader WebAPIConfig m => m Auth.Token
getAppLevelToken = do
  AppLevelToken alt <- asks wacAppLevelToken
  pure $ Auth.Token $ T.encodeUtf8 alt

getBotToken :: MonadReader WebAPIConfig m => m Auth.Token
getBotToken = do
  BotToken bt <- asks wacBotToken
  pure $ Auth.Token $ T.encodeUtf8 bt

endpointFailed :: MonadError WebAPIException m => Text -> SlackResponse key a -> m a
endpointFailed endpoint = \case
  SRSuccess a -> pure a
  SRError err -> throwError $ EndpointFailed endpoint err

data WebAPIException
  = EndpointFailed Text Text
  deriving stock Show

instance Exception WebAPIException where
  displayException = \case
    EndpointFailed endpoint err ->
      [int|s|
        '#{endpoint}' failed:
          #{err}
      |]
