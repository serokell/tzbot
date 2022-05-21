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

To generate app-level / bot tokens, see: <docs/development.md>

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
