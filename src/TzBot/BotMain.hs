-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.BotMain where

import Universum

import Control.Monad.Managed (managed, runManaged)
import Data.Aeson.Types (FromJSON(parseJSON), parseEither)
import Data.Map qualified as M
import Data.Text qualified as T
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Slacker
  (SlackConfig, SocketModeEvent(..), defaultSlackConfig, handleThreadExceptionSensibly,
  pattern EventValue, runSocketMode, setApiToken, setAppToken, setOnException)
import System.Environment (getArgs)

import TzBot.Cache
import TzBot.Config (AppLevelToken(..), BotToken(..), Config(..), readConfig)
import TzBot.ProcessEvent
  (processMemberJoinedChannel, processMemberLeftChannel, processMessageEvent)
import TzBot.RunMonad (BotState(..), log', runBotM)

{- |

Usage:

See @Config.Default.defaultConfigText@ to get what options
are available and how to set them via config or via envvars.
To generate app-level / bot tokens, see: <docs/development.md>

-}
main :: IO ()
main = do
  -- TODO: add optparse
  (configFilePath :: Maybe FilePath) <- safeHead <$> getArgs
  bsConfig@Config{..} <- readConfig configFilePath

  let sCfg = defaultSlackConfig
            & setApiToken (unBotToken cBotToken)
            & setAppToken (unAppLevelToken cAppToken)
            & setOnException handleThreadExceptionSensibly -- auto-handle disconnects

  bsManager <- newManager tlsManagerSettings
  bsMessagesReferences <- newIORef M.empty
  runManaged $ do
    bsUserInfoCache <-
      managed $ withRandomizedCacheDefault cCacheUsersInfo
    bsConversationMembersCache <-
      managed $ withRandomizedCacheDefault cCacheConversationMembers
    liftIO $ runSocketMode sCfg $ handler BotState {..}

handler :: BotState -> SlackConfig -> SocketModeEvent -> IO ()
handler bState _cfg = \e -> do
  case e of
    EventValue "message" evtRaw -> case parseEither parseJSON evtRaw of
      Left err -> log' $ T.pack err
      Right evt -> void . runBotM bState $ processMessageEvent evt
    EventValue "member_joined_channel" evtRaw -> case parseEither parseJSON evtRaw of
      Left err -> log' $ T.pack err
      Right evt -> void . runBotM bState $ processMemberJoinedChannel evt
    EventValue "member_left_channel" evtRaw -> case parseEither parseJSON evtRaw of
      Left err -> log' $ T.pack err
      Right evt -> void . runBotM bState $ processMemberLeftChannel evt
    _ -> pure ()
