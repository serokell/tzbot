-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.BotMain where

import TzPrelude

import Control.Monad.Managed (Managed, managed, runManaged)
import Data.ByteString qualified as BS
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative (execParser)
import Slacker
  (defaultSlackConfig, handleThreadExceptionSensibly, runSocketMode, setApiToken, setAppToken,
  setGracefulShutdownHandler, setOnException)
import System.Directory (doesFileExist)
import System.IO (BufferMode(..), hSetBuffering)
import Text.Interpolation.Nyan (int, rmode')
import Time (hour)

import TzBot.Cache
  (TzCacheSettings(tcsExpiryRandomAmplitudeFraction), defaultTzCacheSettings, withTzCache,
  withTzCacheDefault)
import TzBot.Config
import TzBot.Config.Default (defaultConfigText)
import TzBot.Config.Types (BotConfig)
import TzBot.Logger
import TzBot.Options
import TzBot.ProcessEvents (handler)
import TzBot.RunMonad
import TzBot.Util (withMaybe)

{- |
Usage:

See @Config.Default.defaultConfigText@ to get what options
are available and how to set them via config or via envvars.
To generate app-level / bot tokens, see: <docs/development.md>

-}
main :: IO ()
main = do
  cliOptions <- execParser totalParser
  case cliOptions of
    DumpConfig dumpOpts -> dumpConfig dumpOpts
    DefaultCommand op -> run op

dumpConfig :: DumpOptions -> IO ()
dumpConfig = \case
  DOStdOut -> putStr defaultConfigText
  DOFile path force -> do
    let writeAction = BS.writeFile path defaultConfigText
    if force
    then writeAction
    else ifM
      (doesFileExist path)
      (hPutStrLn @Text stderr [int||File #{path} already exists, \
                                use --force to overwrite|] >> exitFailure)
      writeAction

initBotState :: Options -> Managed BotState
initBotState opts = do
  let mbConfigFilePath = oConfigFile opts
  bsConfig <- liftIO $ readConfig mbConfigFilePath

  let fifteenPercentAmplitudeSettings = defaultTzCacheSettings
        { tcsExpiryRandomAmplitudeFraction = Just 0.15
        }
  bsManager <- liftIO $ newManager tlsManagerSettings
  bsFeedbackConfig <-
    managed $ withFeedbackConfig bsConfig
  bsUserInfoCache <-
    managed $ withTzCache fifteenPercentAmplitudeSettings bsConfig.cCacheUsersInfo
  bsConversationMembersCache <-
    managed $ withTzCache fifteenPercentAmplitudeSettings bsConfig.cCacheConversationMembers
  let defaultMessageInfoCachingTime = hour 1
  bsMessageCache <-
    managed $ withTzCacheDefault defaultMessageInfoCachingTime
  bsMessageLinkCache <-
    managed $ withTzCacheDefault defaultMessageInfoCachingTime
  bsReportEntries <-
    managed $ withTzCacheDefault bsConfig.cCacheReportDialog
  (bsLogNamespace, bsLogContext, bsLogEnv) <- managed $ withLogger bsConfig.cLogLevel
  pure BotState {..}



run :: Options -> IO ()
run opts = do
  runManaged $ do
    botState <- initBotState opts
    gracefulShutdownContainer <- liftIO $ newIORef $ (pure () :: IO ())
    let extractShutdownFunction :: IO () -> IO ()
        extractShutdownFunction = writeIORef gracefulShutdownContainer
    let sCfg = defaultSlackConfig
              & setApiToken (unBotToken botState.bsConfig.cBotToken)
              & setAppToken (unAppLevelToken botState.bsConfig.cAppToken)
              & setOnException handleThreadExceptionSensibly -- auto-handle disconnects
              & setGracefulShutdownHandler extractShutdownFunction

    -- auto-acknowledge received messages
    liftIO $ runSocketMode sCfg $ handler gracefulShutdownContainer botState

withFeedbackConfig :: BotConfig -> (FeedbackConfig -> IO a) -> IO a
withFeedbackConfig config action = do
  let fcFeedbackChannel = config.cFeedbackChannel
  withFeedbackFile config.cFeedbackFile $ \fcFeedbackFile ->
    action FeedbackConfig {..}
  where
    withFeedbackFile :: Maybe FilePath -> (Maybe Handle -> IO a) -> IO a
    withFeedbackFile mbPath action =
      withMaybe mbPath (action Nothing) $ \path ->
        withFile path AppendMode \handle -> do
          -- Use `LineBuffering` so that feedback is flushed to the file immediately.
          hSetBuffering handle LineBuffering
          action $ Just handle
