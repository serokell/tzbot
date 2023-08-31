-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Feedback.Save
  ( FeedbackEntry (..)
  , saveFeedback
  ) where

import TzPrelude

import Data.Aeson (ToJSON, encode)
import Data.List.NonEmpty qualified as NE
import Data.String.Conversions (cs)
import Data.Time (UTCTime)
import Data.Time.TZInfo (TZLabel)
import Data.Time.Zones.All (toTZName)
import Text.Interpolation.Nyan (int, rmode')
import UnliftIO.Exception qualified as UnliftIO

import TzBot.Logger
import TzBot.Render (ConversionPairs, asForOthersS, renderSlackBlocks)
import TzBot.RunMonad
import TzBot.Slack (sendMessage)
import TzBot.Slack.API
import TzBot.Util (RecordWrapper(..))

data FeedbackEntry = FeedbackEntry
  { feMessageText      :: Text
  , feTimeConversion   :: Maybe ConversionPairs
  , feUserReport       :: Text
  , feMessageTimestamp :: UTCTime
  , feSenderTimezone   :: TZLabel
  } deriving stock (Show, Generic)
    deriving ToJSON via RecordWrapper FeedbackEntry

logFeedbackError :: (KatipContext m) => SomeException -> m ()
logFeedbackError (displayException -> err) = do
  logError [int||Error occured while saving user feedback: #{err}|]

-- | Save user feedback to the Slack channel if configured
--   and record to the file if configured.
saveFeedback :: FeedbackEntry -> BotM ()
saveFeedback entry = UnliftIO.handleAny logFeedbackError $ do
  feedbackConfig <- asks bsFeedbackConfig
  whenJust feedbackConfig.fcFeedbackChannel $ saveFeedbackSlack entry
  whenJust feedbackConfig.fcFeedbackFile $ saveFeedbackFile entry

-- Send to the slack channel
saveFeedbackSlack :: FeedbackEntry -> ChannelId -> BotM ()
saveFeedbackSlack entry channelId = sendMessage req
  where
    req = do
      -- We always render the conversion for other users (not author),
      -- so the author can see how their message is converted for others
      let pmrChannel = channelId
          pmrText = "New user feedback"
          pmrBlocks = NE.nonEmpty $
            [ BHeader (Header "Message")
            , BSection $ markdownSection (Mrkdwn $ feMessageText entry)
            , BDivider divider
            , BHeader (Header "Time conversion")
            ] <> renderSlackBlocks asForOthersS (feTimeConversion entry)
            <>
            [ BDivider divider
            , BHeader (Header "Details")
            , BSection $ fieldsSection Nothing $
              ("Message timestamp", show $ feMessageTimestamp entry) :|
              [ ("Sender timezone", Mrkdwn $ cs $ toTZName $ feSenderTimezone entry)
              , ("User report", Mrkdwn $ feUserReport entry)
              ]
            ]
      PostMessageReq {..}

-- Record to the file
saveFeedbackFile :: FeedbackEntry -> Handle -> BotM ()
saveFeedbackFile entry handle = do
  hPutStrLn handle $ encode entry
