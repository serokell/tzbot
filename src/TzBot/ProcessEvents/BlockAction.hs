-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvents.BlockAction
  ( -- * Handlers
    processReportButtonToggled
  , processReportButtonFromEphemeral
  ) where

import Universum

import Data.Aeson (Value)
import Text.Interpolation.Nyan (int, rmode')

import TzBot.Feedback.Dialog (lookupDialogEntry)
import TzBot.Feedback.Dialog.Types
  (ReportDialogEntry(ReportDialogEntry, rpmMessageText, rpmMessageTimestamp, rpmSenderTimeZone, rpmTimeTranslation))
import TzBot.ProcessEvents.Common (openModalCommon)
import TzBot.RunMonad (log')
import TzBot.Slack (BotM, retrieveOneMessage, retrieveOneMessageFromThread, updateModal)
import TzBot.Slack.API (UpdateViewReq(UpdateViewReq))
import TzBot.Slack.Events
import TzBot.Slack.Fixtures qualified as Fixtures
import TzBot.Slack.Modal (mkReportModal)

-- | User was in the view modal and now wants to report error.
--   The input block should be added to collect the user feedback,
--   using `mkReportModal` function.
processReportButtonToggled :: ViewActionEvent Value -> BotM ()
processReportButtonToggled val = do
  let metadataEntryId = vPrivateMetadata $ vaeView val
  mbMetadata <- lookupDialogEntry metadataEntryId
  case mbMetadata of
    Nothing -> log' [int||Dialog id not found: #{metadataEntryId}|]
    Just _metadata@ReportDialogEntry {..} -> updateModal $
      UpdateViewReq
        (mkReportModal rpmMessageText rpmTimeTranslation metadataEntryId)
        (vId $ vaeView val)

-- | User wants to report error, the report modal is started.
processReportButtonFromEphemeral
  :: (ReportEphemeralEvent, Fixtures.MessageIdentifier)
  -> BotM ()
processReportButtonFromEphemeral (evt, Fixtures.MessageIdentifier {..}) = do
  let channelId = scId $ reeChannel evt
  msg <- case miThreadId of
    Nothing -> retrieveOneMessage channelId miMessageId
    Just threadId -> retrieveOneMessageFromThread channelId threadId miMessageId
  let whoTriggeredId = suId $ reeUser evt
      triggerId = reeTriggerId evt
  openModalCommon msg channelId whoTriggeredId triggerId mkReportModal
