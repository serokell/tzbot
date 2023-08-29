-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvents.Interactive
  ( -- * Handlers
    processInteractive
  , processViewSubmission
  ) where

import TzPrelude

import Text.Interpolation.Nyan (int, rmode', rmode's)

import TzBot.Feedback.Dialog (lookupDialogEntry)
import TzBot.Feedback.Dialog.Types
import TzBot.Feedback.Save
import TzBot.Logger
import TzBot.ProcessEvents.Common (openModalCommon)
import TzBot.Render (ConversionPairs)
import TzBot.RunMonad (BotM)
import TzBot.Slack (sendEphemeralMessage)
import TzBot.Slack.API
import TzBot.Slack.Events
import TzBot.Slack.Events.ViewPayload (ViewPayload(unViewPayload))
import TzBot.Slack.Modal (mkReportModal, mkShowModal)

-- | User triggered one of view or report entrypoints,
--   start view or report modal, respectively.
processInteractive :: InteractiveMessageEvent -> BotM ()
processInteractive evt =
  katipAddNamespaceText [int||entrypoint_#s{cbId}|] $ do
  let msg = imeMessage evt
      whoTriggeredId = suId $ imeUser evt
      channelId = scId $ imeChannel evt
      triggerId = imeTriggerId evt
      mkModalFunc :: Text -> Maybe ConversionPairs -> ReportDialogId -> Modal
      mkModalFunc = case imeCallbackId evt of
        CTView   -> mkShowModal
        CTReport -> mkReportModal
  logInfo [int||
    User #{whoTriggeredId} triggered \
    entrypoint #s{cbId} from channel #{channelId}
    |]
  openModalCommon msg channelId whoTriggeredId triggerId mkModalFunc
  where
  cbId = imeCallbackId evt

-- | User entered some report input and submited report view.
--   Collect the input together with the message and timezone
--   information and log it in configured ways (see `saveFeedback`).
processViewSubmission :: SubmitViewEvent -> BotM ()
processViewSubmission (ViewActionEvent view) =
  katipAddNamespaceText "report_view_submitted" $ do
  let metadataEntryId = vPrivateMetadata view
      userInput = unViewPayload $ ufpUserInput $ vState view
  mbMetadata <- lookupDialogEntry metadataEntryId
  case mbMetadata of
    Nothing -> logWarn [int||Dialog id not found: #{metadataEntryId}|]
    Just _metadata@ReportDialogEntry {..} -> do
      logInfo [int||
        Got feedback from user #{rpmUserId}, \
        dialogId #{metadataEntryId}
        |]
      let feedbackEntry = FeedbackEntry
            { feMessageText = rpmMessageText
            , feTimeConversion = rpmTimeConversion
            , feUserReport = userInput
            , feMessageTimestamp = rpmMessageTimestamp
            , feSenderTimezone = rpmSenderTimeZone
            }
      saveFeedback feedbackEntry
      sendEphemeralMessage $ PostEphemeralReq
        { perUser = rpmUserId
        , perChannel = rpmChannelId
        , perThreadTs = rpmThreadId
        , perText = "Thanks for your feedback!"
        , perBlocks = Nothing
        }
