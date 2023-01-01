-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Slack.Fixtures where

import Data.Aeson
import Data.Text qualified as T
import TzBot.Slack.API
import TzBot.Util
import Universum

viewEntrypointCallbackId :: Text
viewEntrypointCallbackId = "tz_view"

reportEntrypointCallbackId :: Text
reportEntrypointCallbackId = "tz_report"

---------------------------
-- block action identifiers
---------------------------

-- message view modal, "report" button
reportButtonActionId :: ActionId
reportButtonActionId = "report-button-action-id"

reportButtonBlockId :: BlockId
reportButtonBlockId = "report-button-block-id"

-- report view, plain text input block
reportInputElementActionId :: ActionId
reportInputElementActionId = "report-input-element-action-id"

reportInputBlockId :: BlockId
reportInputBlockId = "report-input-block-id"

-- report button from the ephemeral message
data MessageIdentifier = MessageIdentifier
  { miMessageId :: MessageId
  , miThreadId :: Maybe ThreadId
  } deriving stock (Generic)
    deriving (FromJSON, ToJSON) via RecordWrapper MessageIdentifier

reportFromEphemeralActionIdPrefix :: Text
reportFromEphemeralActionIdPrefix = "report-from-ephemeral-action-id"

reportFromEphemeralActionId :: MessageId -> Maybe ThreadId -> ActionId
reportFromEphemeralActionId msgId mbThreadId = do
  ActionId $
    reportFromEphemeralActionIdPrefix
      <> encodeText (MessageIdentifier msgId mbThreadId)

reportFromEphemeralBlockId :: BlockId
reportFromEphemeralBlockId = "report-from-ephemeral-block-id"

getMessageIdFromReportEphemeral :: ActionId -> Maybe MessageIdentifier
getMessageIdFromReportEphemeral (ActionId aId) = do
  suf <- T.stripPrefix reportFromEphemeralActionIdPrefix aId
  decodeText suf

---------------------------
-- modal types
---------------------------
viewModal :: Text
viewModal = "view_modal"

reportModal :: Text
reportModal = "report_modal"
