-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.BotMain where

import Universum

import Data.Aeson.Types (FromJSON(parseJSON), parseEither)
import Data.Map qualified as M
import Data.Text qualified as T
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Slacker
  (SlackConfig, SocketModeEvent(..), defaultSlackConfig, handleThreadExceptionSensibly,
  pattern EventValue, runSocketMode, setApiToken, setAppToken, setOnException)
import System.Environment (getArgs)

import TzBot.Config (AppLevelToken(..), BotToken(..), Config(cAppToken, cBotToken), readConfig)
import TzBot.ProcessEvent (processEvent)
import TzBot.RunMonad (BotState(BotState), log', runBotM)

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
  config <- readConfig configFilePath

  let appLevelToken = cAppToken config
      botToken = cBotToken config
  let sCfg = defaultSlackConfig
            & setApiToken (unBotToken botToken)
            & setAppToken (unAppLevelToken appLevelToken)
            & setOnException handleThreadExceptionSensibly -- auto-handle disconnects

  manager <- newManager tlsManagerSettings
  refSetMapIORef <- newIORef M.empty
  let bState = BotState config manager refSetMapIORef

  runSocketMode sCfg (handler bState) -- auto-acknowledge received messages

handler :: BotState -> SlackConfig -> SocketModeEvent -> IO ()
handler bState _cfg = \e -> do
  case e of
    EventValue "message" evtRaw -> case parseEither parseJSON evtRaw of
      Left err -> log' $ T.pack err
      Right evt -> void . runBotM bState $ processEvent evt
    _ -> pure ()
