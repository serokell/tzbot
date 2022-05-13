module Main where

import Data.Text qualified as T
import System.Environment (getEnv)
import TzBot.Slack.SocketMode.Client as WS
import TzBot.Slack.WebAPI.Impl (AppLevelToken(..), WebAPIConfig(..))

{- |

Usage:

>  export SLACK_APP_TOKEN='xapp-***'
>  stack run

You can find your app-level token by going to your Slack App settings and:
  Basic Information > App-Level Tokens

-}
main :: IO ()
main = do
  appLevelToken <- getEnv "SLACK_APP_TOKEN"
  let webApiConfig = WebAPIConfig
        { wacAppLevelToken = AppLevelToken $ T.pack appLevelToken
        }
  WS.wsMain webApiConfig
