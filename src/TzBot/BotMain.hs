-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.BotMain where

import Universum

import Data.Aeson.Types
import Data.Map qualified as M
import Data.Text qualified as T
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Slacker
  (SlackConfig, SocketModeEvent(..), defaultSlackConfig, handleThreadExceptionSensibly,
  pattern EventValue, runSocketMode, setApiToken, setAppToken, setOnException)
import System.Environment (getEnv)

import TzBot.ProcessEvent (processEvent)
import TzBot.RunMonad

{- |

Usage:

>  export SLACK_TZ_APP_TOKEN='xapp-***'
>  export SLACK_TZ_BOT_TOKEN='xoxb-***'
>  stack run

To generate app-level / bot tokens, see: <docs/development.md>

-}
main :: IO ()
main = do
  appLevelToken <- T.pack <$> getEnv "SLACK_TZ_APP_TOKEN"
  botToken <- T.pack <$> getEnv "SLACK_TZ_BOT_TOKEN"
  let sCfg = defaultSlackConfig
            & setApiToken botToken
            & setAppToken appLevelToken
            & setOnException handleThreadExceptionSensibly -- auto-handle disconnects

  let wCfg = BotConfig {
    bcAppLevelToken = AppLevelToken appLevelToken
  , bcBotToken = BotToken botToken
  }

  manager <- newManager tlsManagerSettings
  refSetMapIORef <- newIORef M.empty
  let wState = BotState wCfg manager refSetMapIORef

  runSocketMode sCfg (handler wState) -- auto-acknowledge received messages

handler :: BotState -> SlackConfig -> SocketModeEvent -> IO ()
handler wstate _cfg = \e -> do
  case e of
    EventValue "message" evtRaw -> case parseEither parseJSON evtRaw of
      Left err -> log' $ T.pack err
      Right evt -> void . runBotM wstate $ processEvent evt
    _ -> pure ()
