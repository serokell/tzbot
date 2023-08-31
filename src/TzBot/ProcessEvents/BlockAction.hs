-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvents.BlockAction
  ( -- * Handlers
    processReportButtonToggled
  ) where

import TzPrelude

import Data.Aeson (Value)
import Text.Interpolation.Nyan (int, rmode')

import TzBot.Feedback.Dialog (lookupDialogEntry)
import TzBot.Feedback.Dialog.Types (ReportDialogEntry(..))
import TzBot.Logger
import TzBot.Slack (BotM, updateModal)
import TzBot.Slack.API (UpdateViewReq(UpdateViewReq))
import TzBot.Slack.Events
import TzBot.Slack.Modal (mkReportModal)

-- | User was in the view modal and now wants to report error.
--   The input block should be added to collect the user feedback,
--   using `mkReportModal` function.
processReportButtonToggled :: ViewActionEvent Value -> BotM ()
processReportButtonToggled val =
  katipAddNamespaceText "report_button_toggled" $ do
  logInfo [int||Report button of view #{vId $ vaeView val} toggled|]
  let metadataEntryId = vPrivateMetadata $ vaeView val
  mbMetadata <- lookupDialogEntry metadataEntryId
  case mbMetadata of
    Nothing -> logWarn [int||Dialog id not found: #{metadataEntryId}|]
    Just reportDialogEntry -> updateModal $
      UpdateViewReq
        (mkReportModal reportDialogEntry.rpeMessageText reportDialogEntry.rpeTimeConversion metadataEntryId)
        (vId $ vaeView val)
