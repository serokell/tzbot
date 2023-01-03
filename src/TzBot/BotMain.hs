-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.BotMain where

import Universum

import Control.Monad.Managed (managed, runManaged)
import Data.ByteString qualified as BS
import Data.Map qualified as M
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative (execParser)
import Slacker
  (defaultSlackConfig, handleThreadExceptionSensibly, runSocketMode, setApiToken, setAppToken,
  setOnException)
import System.Directory (doesFileExist)
import Text.Interpolation.Nyan (int, rmode')

import TzBot.Cache
  (TzCacheSettings(tcsExpiryRandomAmplitudeFraction), defaultTzCacheSettings, withTzCache,
  withTzCacheDefault)
import TzBot.Config
import TzBot.Config.Default (defaultConfigText)
import TzBot.Config.Types (BotConfig)
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

run :: Options -> IO ()
run opts = do
  let mbConfigFilePath = oConfigFile opts
  bsConfig@Config {..} <- readConfig mbConfigFilePath
  runManaged $ do

    let fifteenPercentAmplitudeSettings = defaultTzCacheSettings
          { tcsExpiryRandomAmplitudeFraction = Just 0.15
          }
    let sCfg = defaultSlackConfig
              & setApiToken (unBotToken cBotToken)
              & setAppToken (unAppLevelToken cAppToken)
              & setOnException handleThreadExceptionSensibly -- auto-handle disconnects

    bsManager <- liftIO $ newManager tlsManagerSettings
    bsMessagesReferences <- newIORef M.empty
    bsFeedbackConfig <-
      managed $ withFeedbackConfig bsConfig
    bsUserInfoCache <-
      managed $ withTzCache fifteenPercentAmplitudeSettings cCacheUsersInfo
    bsConversationMembersCache <-
      managed $ withTzCache fifteenPercentAmplitudeSettings cCacheConversationMembers
    bsReportEntries <-
      managed $ withTzCacheDefault cCacheReportDialog
    -- auto-acknowledge received messages
    liftIO $ runSocketMode sCfg $ handler BotState {..}

withFeedbackConfig :: BotConfig -> (FeedbackConfig -> IO a) -> IO a
withFeedbackConfig Config {..} action = do
  let fcFeedbackChannel = cFeedbackChannel
  withFeedbackFile cFeedbackFile $ \fcFeedbackFile ->
    action FeedbackConfig {..}
  where
    withFeedbackFile :: Maybe FilePath -> (Maybe Handle -> IO a) -> IO a
    withFeedbackFile mbPath action =
      withMaybe mbPath (action Nothing) $ \path ->
        withFile path AppendMode (action . Just)
