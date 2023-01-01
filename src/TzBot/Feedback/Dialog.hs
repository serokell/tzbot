-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Feedback.Dialog where

import Universum

import Data.Map qualified as Map

import TzBot.Feedback.Dialog.Types
import TzBot.RunMonad

insertDialogEntry :: ReportDialogId -> ReportDialogEntry -> BotM ()
insertDialogEntry id_ entry = do
  ref <- asks bsReportEntries
  atomicModifyIORef' ref $ \x ->
    (Map.insert id_ entry x, ())

lookupDialogEntry :: ReportDialogId -> BotM (Maybe ReportDialogEntry)
lookupDialogEntry id_ = Map.lookup id_ <$> do
  asks bsReportEntries >>= readIORef
