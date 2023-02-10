-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.RunMonad where

import Universum

import Control.Lens (makeLensesWith)
import Data.Set qualified as S
import Katip qualified as K
import Network.HTTP.Client (Manager)
import Text.Interpolation.Nyan (int, rmode')
import UnliftIO (MonadUnliftIO)

import TzBot.Cache (TzCache)
import TzBot.Config.Types (BotConfig)
import TzBot.Feedback.Dialog.Types (ReportDialogEntry, ReportDialogId)
import TzBot.Slack.API
import TzBot.TimeReference
import TzBot.Util (postfixFields)

data FeedbackConfig = FeedbackConfig
  { fcFeedbackChannel :: Maybe ChannelId
  , fcFeedbackFile    :: Maybe Handle
  }

data BotState = BotState
  { bsConfig         :: BotConfig
  , bsManager        :: Manager
  , bsFeedbackConfig :: FeedbackConfig
  , bsUserInfoCache  :: TzCache UserId User
  , bsConversationMembersCache :: TzCache ChannelId (S.Set UserId)
  , bsReportEntries  :: TzCache ReportDialogId ReportDialogEntry
  , bsMessageCache   :: TzCache MessageId [TimeReference]
  , bsMessageLinkCache :: TzCache MessageId Text
  , bsLogNamespace   :: K.Namespace
  , bsLogContext     :: K.LogContexts
  , bsLogEnv         :: K.LogEnv
  }
makeLensesWith postfixFields ''BotState

newtype BotM a = BotM
  { unBotM :: ReaderT BotState IO a
  }
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader BotState
    , MonadIO, MonadUnliftIO
    )

runBotM :: BotState -> BotM a -> IO a
runBotM state action =
  action
    & unBotM
    & flip runReaderT state

instance K.Katip BotM where
  localLogEnv f = local (over bsLogEnvL f)
  getLogEnv = view bsLogEnvL

instance K.KatipContext BotM where
  localKatipContext f = local (over bsLogContextL f)
  localKatipNamespace f = local (over bsLogNamespaceL f)
  getKatipContext = view bsLogContextL
  getKatipNamespace = view bsLogNamespaceL

runKatipWithBotState :: BotState -> K.KatipContextT m a -> m a
runKatipWithBotState BotState {..} action =
  K.runKatipContextT bsLogEnv bsLogContext bsLogNamespace action
----------------------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------------------

type FunctionName = Text
type EndpointName = Text
type ErrorDescription = Text

data BotException
  = EndpointFailed EndpointName ErrorDescription
  | UnexpectedResult EndpointName FunctionName ErrorDescription
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
