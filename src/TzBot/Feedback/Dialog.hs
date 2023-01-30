-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Feedback.Dialog
  ( -- * View/report dialog state
    --
    -- | Functions that are used for keeping intermediate state
    --   of the view/report dialog.
    insertDialogEntry
  , lookupDialogEntry
  ) where

import Universum

import TzBot.Cache qualified as Cache
import TzBot.Feedback.Dialog.Types (ReportDialogEntry, ReportDialogId)
import TzBot.RunMonad (BotM, BotState(bsReportEntries))

insertDialogEntry :: ReportDialogId -> ReportDialogEntry -> BotM ()
insertDialogEntry id_ entry = do
  dialogEntriesCache <- asks bsReportEntries
  Cache.insert id_ entry dialogEntriesCache

lookupDialogEntry :: ReportDialogId -> BotM (Maybe ReportDialogEntry)
lookupDialogEntry id_ = do
  dialogEntriesCache <- asks bsReportEntries
  Cache.lookup id_ dialogEntriesCache
