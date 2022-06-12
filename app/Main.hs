-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Main where

import Control.Monad (void)
import Data.Function ((&))
import Data.Text qualified as T
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getEnv)

import Slacker
  (SlackConfig, SocketModeEvent(..), defaultSlackConfig, handleThreadExceptionSensibly,
  pattern Event, runSocketMode, setApiToken, setAppToken, setOnException)

import TzBot.ProcessEvent (constructEventSummary, processEvent)
import TzBot.Slack.WebAPI.Impl
  (AppLevelToken(..), BotToken(..), WebAPIConfig(..), WebAPIState(..), runWebAPIM)

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

  let wCfg = WebAPIConfig {
    wacAppLevelToken = AppLevelToken appLevelToken
  , wacBotToken = BotToken botToken
  }

  manager <- newManager tlsManagerSettings
  let wState = WebAPIState wCfg manager

  runSocketMode sCfg (handler wState) -- auto-acknowledge received messages

handler :: WebAPIState -> SlackConfig -> SocketModeEvent -> IO ()
handler wstate _cfg = \case
  Event "message" evt -> void . runWebAPIM wstate $ processEvent (constructEventSummary evt)
  _ -> pure ()
