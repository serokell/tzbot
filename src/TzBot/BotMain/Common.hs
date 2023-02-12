-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.BotMain.Common where

import Universum

import Control.Monad.Managed (Managed, managed)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Time (hour)

import TzBot.Cache
  (TzCacheSettings(tcsExpiryRandomAmplitudeFraction), defaultTzCacheSettings, withTzCache,
  withTzCacheDefault)
import TzBot.Config
import TzBot.Config.Types (BotConfig)
import TzBot.Logger
import TzBot.RunMonad
import TzBot.Util

withBotState :: BotConfig -> Managed BotState
withBotState bsConfig@Config {..} = do
  let fifteenPercentAmplitudeSettings = defaultTzCacheSettings
        { tcsExpiryRandomAmplitudeFraction = Just 0.15
        }

  bsManager <- liftIO $ newManager tlsManagerSettings
  bsFeedbackConfig <-
    managed $ withFeedbackConfig bsConfig
  bsUserInfoCache <-
    managed $ withTzCache fifteenPercentAmplitudeSettings cCacheUsersInfo

  bsConversationMembersCache <-
    managed $ withTzCache fifteenPercentAmplitudeSettings cCacheConversationMembers
  let defaultMessageInfoCachingTime = hour 1
  bsMessageCache <-
    managed $ withTzCacheDefault defaultMessageInfoCachingTime
  bsMessageLinkCache <-
    managed $ withTzCacheDefault defaultMessageInfoCachingTime
  bsReportEntries <-
    managed $ withTzCacheDefault cCacheReportDialog
  -- auto-acknowledge received messages
  (bsLogNamespace, bsLogContext, bsLogEnv) <- managed $ withLogger cLogLevel
  pure BotState {..}

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
