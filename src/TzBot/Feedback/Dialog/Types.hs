-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Feedback.Dialog.Types where

import Universum

import Data.Aeson
import Data.Time
import Data.Time.Zones.All (TZLabel)
import Formatting

import TzBot.Instances ()
import TzBot.Render (TranslationPairs)

newtype ReportDialogId = ReportDialogId { unReportDialogId :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, Buildable)

data ReportDialogEntry = ReportDialogEntry
  { rpmMessageText :: Text
  , rpmTimeTranslation :: Maybe TranslationPairs
  , rpmSenderTimeZone :: TZLabel
  , rpmMessageTimestamp :: UTCTime
  } deriving stock (Eq, Show, Generic)
