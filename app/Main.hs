module Main where

import Data.Text qualified as T
import System.Environment (getEnv)
import TzBot.Slack.SocketMode.Client as WS
import TzBot.Slack.WebAPI.Impl (AppLevelToken(..), BotToken(..), WebAPIConfig(..))

{- |

Usage:

>  export SLACK_TZ_APP_TOKEN='xapp-***'
>  export SLACK_TZ_BOT_TOKEN='xoxb-***'
>  stack run

You can find your app-level token by going to your
Slack App settings (at @https://api.slack.com/apps/@) and:
  Basic Information > App-Level Tokens

You can find your bot token by going to:
  OAuth & Permissions > OAuth Tokens for Your Workspace
-}
main :: IO ()
main = do
  appLevelToken <- getEnv "SLACK_TZ_APP_TOKEN"
  botToken <- getEnv "SLACK_TZ_BOT_TOKEN"
  let webApiConfig = WebAPIConfig
        { wacAppLevelToken = AppLevelToken $ T.pack appLevelToken
        , wacBotToken = BotToken $ T.pack botToken
        }
  WS.wsMain webApiConfig
