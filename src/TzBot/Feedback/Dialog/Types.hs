-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Feedback.Dialog.Types
  ( ReportDialogId (..)
  , ReportDialogEntry (..)
  ) where

import TzPrelude

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Data.Time.Zones.All (TZLabel)
import Formatting (Buildable)

import TzBot.Instances ()
import TzBot.Render (ConversionPairs)
import TzBot.Slack.API (ChannelId, ThreadId, UserId)

newtype ReportDialogId = ReportDialogId { unReportDialogId :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, Buildable, Hashable)

data ReportDialogEntry = ReportDialogEntry
  { rpmMessageText :: Text
  , rpmTimeConversion :: Maybe ConversionPairs
  , rpmSenderTimeZone :: TZLabel
  , rpmMessageTimestamp :: UTCTime
  , rpmUserId :: UserId
  , rpmChannelId :: ChannelId
  , rpmThreadId :: Maybe ThreadId
  } deriving stock (Eq, Show, Generic)
