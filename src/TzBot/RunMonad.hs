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

import TzBot.Cache (TzCache)
import TzBot.Config.Types (BotConfig)
import TzBot.Feedback.Dialog.Types (ReportDialogEntry, ReportDialogId)
import TzBot.Slack.API (ChannelId, MessageId, User, UserId)
import TzBot.TimeReference (TimeReferenceText)

data FeedbackConfig = FeedbackConfig
  { fcFeedbackChannel :: Maybe ChannelId
  , fcFeedbackFile    :: Maybe Handle
  }

data BotState = BotState
  { bsConfig :: BotConfig
  , bsManager :: Manager
  , bsFeedbackConfig :: FeedbackConfig
  -- TODO: after #22 bsMessagesReferences should either disappear or become
  -- cached (not IORef).
  , bsMessagesReferences :: IORef (M.Map MessageId (S.Set TimeReferenceText))
  , bsUserInfoCache :: TzCache UserId User
  , bsConversationMembersCache :: TzCache ChannelId (S.Set UserId)
  , bsReportEntries :: TzCache ReportDialogId ReportDialogEntry
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

type FunctionName = Text
type EndpointName = Text
type ErrorDescription = Text

data BotException
  = EndpointFailed EndpointName ErrorDescription
  | UnexpectedResult EndpointName FunctionName ErrorDescription
  | ServantError ClientError
  deriving stock (Show, Generic)

instance Exception BotException where
  displayException = \case
    EndpointFailed endpoint err ->
      [int|s|
        '#{endpoint}' failed: \
          #{err}
      |]
    UnexpectedResult endpoint funcName err ->
      [int|s|
        '#{funcName}', unexpected result from endpoint '#{endpoint}': #{err}
      |]
    ServantError clientError ->
      displayException clientError
