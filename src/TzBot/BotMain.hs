-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.BotMain where

import Universum

import Data.Map qualified as M
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Slacker
  (SlackConfig, SocketModeEvent(..), defaultSlackConfig, handleThreadExceptionSensibly,
  pattern BlockAction, pattern EventValue, pattern Interactive, runSocketMode, setApiToken,
  setAppToken, setOnException)
import System.Environment (getArgs)

import Text.Interpolation.Nyan
import TzBot.Config (AppLevelToken(..), BotToken(..), Config(..), ConfigFinal, readConfig)
import TzBot.ProcessEvents.BlockAction
import TzBot.ProcessEvents.Interactive
import TzBot.ProcessEvents.Message (processMessageEvent)
import TzBot.RunMonad
import TzBot.Util

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

  withFeedbackConfig config $ \feedbackConfig -> do

    let appLevelToken = cAppToken config
        botToken = cBotToken config
    let sCfg = defaultSlackConfig
              & setApiToken (unBotToken botToken)
              & setAppToken (unAppLevelToken appLevelToken)
              & setOnException handleThreadExceptionSensibly -- auto-handle disconnects

    let wCfg = BotConfig
          { bcAppLevelToken = appLevelToken
          , bcBotToken = botToken
          }

    manager <- newManager tlsManagerSettings
    refSetMapIORef <- newIORef M.empty
    dialogMapIORef <- newIORef M.empty
    let wState = BotState wCfg manager feedbackConfig refSetMapIORef dialogMapIORef

    runSocketMode sCfg (handler wState) -- auto-acknowledge received messages

withFeedbackConfig :: ConfigFinal -> (FeedbackConfig -> IO a) -> IO a
withFeedbackConfig Config {..} action = do
  fcFeedbackFile <- traverse (\path -> openFile path AppendMode) cFeedbackFile
  let fcFeedbackChannel = cFeedbackChannel
  action FeedbackConfig {..} `finally` whenJust fcFeedbackFile hClose

handler :: BotState -> SlackConfig -> SocketModeEvent -> IO ()
handler wstate _cfg = \e -> do
  run $ case e of
    EventValue "message" evtRaw
      | Just evt <- decodeMaybe evtRaw -> processMessageEvent evt

    Interactive interactiveType interactiveRaw
      | Just interactiveAction <- getInteractive interactiveType interactiveRaw ->
        case interactiveAction of
          IEMessageEvent ime -> processInteractive ime
          IEReportViewSubmitted (viewSubmittedEvent, input)
            -> processViewSubmission input viewSubmittedEvent

    BlockAction actionId blockActionRaw
      | Just blockActionEvent <- getBlockAction actionId blockActionRaw ->
        case blockActionEvent of
          BAReportButtonToggled reportButtonToggledEvent
            -> processReportButtonToggled reportButtonToggledEvent
          BAReportButtonFromEphemeral reportButtonEphemeralEvent
            -> processReportButtonFromEphemeral reportButtonEphemeralEvent
    _ -> pure ()
  where
    run :: BotM a -> IO ()
    run action = do
      eithRes <- runBotM wstate action
      case eithRes of
        Left err -> log' [int||Error occured: #{show @Text err}|]
        Right _ -> pure ()
