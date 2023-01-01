-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Feedback.Save where

import Universum

import Control.Monad.Error.Class
import Data.Aeson
import Data.List.NonEmpty qualified as NE
import Data.String.Conversions
import Data.Time
import Data.Time.TZInfo
import Data.Time.Zones.All (toTZName)
import Text.Interpolation.Nyan

import TzBot.Render (TranslationPairs, renderSlackBlock)
import TzBot.RunMonad
import TzBot.Slack (sendMessage)
import TzBot.Slack.API
import TzBot.Util

data FeedbackEntry = FeedbackEntry
  { feMessageText :: Text
  , feTimeTranslation :: Maybe TranslationPairs
  , feUserReport :: Text
  , feMessageTimestamp :: UTCTime
  , feSenderTimezone :: TZLabel
  } deriving stock (Show, Generic)
    deriving ToJSON via RecordWrapper FeedbackEntry

logFeedbackError :: BotException -> BotM ()
logFeedbackError (displayException -> err) = do
  log' [int||Error occured while saving user feedback: #{err}|]

saveFeedback :: FeedbackEntry -> BotM ()
saveFeedback entry = flip catchError logFeedbackError $ do
  FeedbackConfig {..} <- asks bsFeedbackConfig
  whenJust fcFeedbackChannel $ saveFeedbackSlack entry
  whenJust fcFeedbackFile $ saveFeedbackFile entry

-- Send to the slack channel
saveFeedbackSlack :: FeedbackEntry -> ChannelId -> BotM ()
saveFeedbackSlack entry channelId = sendMessage req
  where
    req = do
      let pmrChannel = channelId
          pmrText = "New user feedback"
          pmrBlocks = NE.nonEmpty
            [ BHeader (Header "Message")
            , BSection (textSection (PlainText $ feMessageText entry) Nothing)
            , BDivider divider
            , BHeader (Header "Time translation")
            , renderSlackBlock $ feTimeTranslation entry
            , BDivider divider
            , BHeader (Header "Details")
            , BSection $ fieldsSection Nothing Nothing $
              ("Message timestamp", show $ feMessageTimestamp entry) :|
              [ ("Sender timezone", PlainText $ cs $ toTZName $ feSenderTimezone entry)
              , ("User report", PlainText $ feUserReport entry)
              ]
            ]
      PostMessageReq {..}

-- Record to the file
saveFeedbackFile :: FeedbackEntry -> Handle -> BotM ()
saveFeedbackFile entry handle = do
  hPutStrLn handle $ encode entry
