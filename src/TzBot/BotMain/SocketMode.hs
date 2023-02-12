-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.BotMain.SocketMode where

import Universum

import Control.Exception (AsyncException(UserInterrupt))
import Control.Monad.Managed (runManaged)
import Slacker
  (DisconnectBody(..), EventsApiEnvelope(..), HelloBody(..), SlashCommandsEnvelope(..),
  SocketModeEvent(..), defaultSlackConfig, handleThreadExceptionSensibly, pattern BlockAction,
  pattern Command, pattern EventValue, pattern Interactive, runSocketMode, setApiToken, setAppToken,
  setGracefulShutdownHandler, setOnException)
import Slacker.SocketMode (InteractiveEnvelope(..))
import Text.Interpolation.Nyan (int, rmode', rmode's)
import UnliftIO.Exception qualified as UnliftIO

import TzBot.BotMain.Common
import TzBot.Config
import TzBot.Logger
import TzBot.Options
import TzBot.ProcessEvents
  (handleRawBlockAction, handleRawEvent, handleRawInteractive, handleSlashCommand)
import TzBot.RunMonad (BotM, BotState, runBotM)

runSocketMode :: RunSocketModeOptions -> IO ()
runSocketMode opts = do
  let mbConfigFilePath = rsmoConfigFile opts
  bsConfig@Config {..} <- readConfig mbConfigFilePath
  runManaged $ do

    gracefulShutdownContainer <- liftIO $ newIORef $ (pure () :: IO ())
    let extractShutdownFunction :: IO () -> IO ()
        extractShutdownFunction = writeIORef gracefulShutdownContainer
    let sCfg = defaultSlackConfig
              & setApiToken (unBotToken cBotToken)
              & setAppToken (unAppLevelToken cAppToken)
              & setOnException handleThreadExceptionSensibly -- auto-handle disconnects
              & setGracefulShutdownHandler extractShutdownFunction
    botState <- withBotState bsConfig
    liftIO $ Slacker.runSocketMode sCfg \_ e ->
      run gracefulShutdownContainer botState $ socketModeHandler e
  where
  run :: IORef (IO ()) -> BotState -> BotM a -> IO ()
  run shutdownRef bState action = void $ runBotM bState $ do
    eithRes <- UnliftIO.trySyncOrAsync action
    whenLeft eithRes $ \e -> do
      case fromException e of
        Just UserInterrupt -> liftIO $ join $ readIORef shutdownRef
        _ -> logError [int||Error occured: #{displayException e}|]

socketModeHandler :: SocketModeEvent -> BotM ()
socketModeHandler e = do
  logDebug [int||Received Slack event: #{show @Text e}|]
  case e of
    Command _cmdType slashCmd -> handleSlashCommand slashCmd

    EventValue eventType evtRaw -> handleRawEvent envelopeIdentifier eventType evtRaw

    -- BlockAction events form a subset of Interactive, so check them first
    BlockAction actionId blockActionRaw ->
      handleRawBlockAction envelopeIdentifier actionId blockActionRaw

    Interactive interactiveType interactiveRaw ->
      handleRawInteractive envelopeIdentifier interactiveType interactiveRaw
    _ -> logWarn [int||Unknown SocketModeEvent #s{e}|]
  where
    envelopeIdentifier :: Text
    envelopeIdentifier = case e of
      EventsApi EventsApiEnvelope {..} -> eaeEnvelopeId
      SlashCommands SlashCommandsEnvelope {..} -> sceEnvelopeId
      InteractiveEvent InteractiveEnvelope {..} -> ieEnvelopeId
      Hello HelloBody {} -> "hello_body"
      Disconnect DisconnectBody {} -> "disconnect_body"
