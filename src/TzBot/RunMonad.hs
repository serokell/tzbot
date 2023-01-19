-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.RunMonad where

import Universum

import Control.Monad.Base (MonadBase)
import Control.Monad.Except (MonadError)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text.IO qualified as T
import Network.HTTP.Client (Manager)
import Servant.Client (ClientError)
import Text.Interpolation.Nyan (int, rmode')

import TzBot.Cache1 (RandomizedCache)
import TzBot.Config.Types (BotConfig)
import TzBot.Slack.API (ChannelId, MessageId, User, UserId)
import TzBot.TimeReference (TimeReferenceText)

data BotState = BotState
  { bsConfig :: BotConfig
  , bsManager :: Manager
  , bsMessagesReferences :: IORef (M.Map MessageId (S.Set TimeReferenceText))
  , bsUserInfoCache :: RandomizedCache UserId User
  , bsConversationMembersCache :: RandomizedCache ChannelId (S.Set UserId)
  }

newtype BotM a = BotM
  { unBotM :: ReaderT BotState (ExceptT BotException IO) a
  }
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader BotState, MonadError BotException
    , MonadIO, MonadBaseControl IO, MonadBase IO
    )

runBotM :: BotState -> BotM a -> IO (Either BotException a)
runBotM state action =
  action
    & unBotM
    & flip runReaderT state
    & runExceptT

runOrThrowBotM :: BotState -> BotM a -> IO a
runOrThrowBotM state action =
  runBotM state action >>= either throwM pure

-- This ugly logging is only temporary
log' :: MonadIO m => Text -> m ()
log' msg = liftIO $ T.putStrLn $ "Bot> " <> msg
----------------------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------------------

data BotException
  = EndpointFailed Text Text
  | ServantError ClientError
  deriving stock Show

instance Exception BotException where
  displayException = \case
    EndpointFailed endpoint err ->
      [int|s|
        '#{endpoint}' failed:
          #{err}
      |]
    ServantError clientError ->
      displayException clientError
