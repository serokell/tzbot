-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvents.Interactive (
  InteractiveEvent (..),
  InteractiveMessageEvent (..),
  getInteractive,
  processInteractive,
  processViewSubmission
  ) where

import Universum

import Data.Aeson
import Data.Aeson.Lens

import Text.Interpolation.Nyan
import TzBot.Feedback.Dialog
import TzBot.Feedback.Dialog.Types
import TzBot.Feedback.Save
import TzBot.ProcessEvents.Common
import TzBot.Render
import TzBot.RunMonad (log')
import TzBot.Slack
import TzBot.Slack.API
import TzBot.Slack.Events
import TzBot.Slack.Fixtures qualified as Fixtures
import TzBot.Slack.Modal
import TzBot.Util

-- Routing
data InteractiveEvent =
  IEMessageEvent InteractiveMessageEvent
  | IEReportViewSubmitted (View, Text)

getInteractive :: Text -> Value -> Maybe InteractiveEvent
getInteractive actionType raw = do
  asum
    [ IEMessageEvent <$> getMessageAction actionType raw
    , IEReportViewSubmitted <$> getSubmitReportViewEvent actionType raw
    ]

getMessageAction :: Text -> Value -> Maybe InteractiveMessageEvent
getMessageAction actionType val = do
  guard (actionType == "message_action")
  decodeMaybe val

getSubmitReportViewEvent :: Text -> Value -> Maybe (View, Text)
getSubmitReportViewEvent interactiveType raw = do
  guard (interactiveType == "view_submission")
  event <- decodeMaybe raw
  let view = vaeView event
  guard (vCallbackId view == Fixtures.reportModal)
  userInput <- getUserInput $ vState $ vaeView event
  pure (view, userInput)

getUserInput :: Value -> Maybe Text
getUserInput val =
  val ^? key "values"
    . key (unBlockId Fixtures.reportInputBlockId)
    . key (unActionId Fixtures.reportInputElementActionId)
    . key "value"
    . _String

-- Handlers
processInteractive :: InteractiveMessageEvent -> BotM ()
processInteractive evt = do
  let msg = imeMessage evt
      whoTriggeredId = suId $ imeUser evt
      triggerId = imeTriggerId evt
      mkModalFunc :: Text -> Maybe TranslationPairs -> ReportDialogId -> Modal
      mkModalFunc = case imeCallbackId evt of
        CTView -> mkShowModal
        CTReport -> mkReportModal
  openModalCommon msg whoTriggeredId triggerId mkModalFunc

processViewSubmission :: Text -> View -> BotM ()
processViewSubmission userInput view = do
  let metadataEntryId = vPrivateMetadata view
  mbMetadata <- lookupDialogEntry metadataEntryId
  case mbMetadata of
    Nothing -> log' [int||Dialog id not found: #{metadataEntryId}|]
    Just _metadata@ReportDialogEntry {..} -> do
      let feedbackEntry = FeedbackEntry
            { feMessageText = rpmMessageText
            , feTimeTranslation = rpmTimeTranslation
            , feUserReport = userInput
            , feMessageTimestamp = rpmMessageTimestamp
            , feSenderTimezone = rpmSenderTimeZone
            }
      saveFeedback feedbackEntry
