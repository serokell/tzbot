-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Slack.Fixtures
  ( -- * Entrypoints callback_ids
    pattern ViewEntrypointCallbackId
  , pattern ReportEntrypointCallbackId

  -- * Block action_ids
  , reportButtonActionId
  , reportButtonBlockId
  , ReportInputElementActionId
  , ReportInputBlockId
  , reportInputElementActionId
  , reportInputBlockId

  -- * Report button inside ephemerals
  , MessageIdentifier (..)
  , reportFromEphemeralActionIdPrefix
  , reportFromEphemeralActionId
  , getMessageIdFromReportEphemeral
  , reportFromEphemeralBlockId

  -- * Modal identifiers
  , viewModal
  , reportModal
  ) where

import Universum

import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import GHC.TypeLits (symbolVal)

import TzBot.Slack.API (ActionId(ActionId), BlockId, MessageId, ThreadId)
import TzBot.Util (RecordWrapper(..), decodeText, encodeText)

pattern ViewEntrypointCallbackId :: Text
pattern ViewEntrypointCallbackId = "tz_view"

pattern ReportEntrypointCallbackId :: Text
pattern ReportEntrypointCallbackId = "tz_report"

---------------------------
-- block action identifiers
---------------------------

-- message view modal, "report" button
reportButtonActionId :: ActionId
reportButtonActionId = "report-button-action-id"

reportButtonBlockId :: BlockId
reportButtonBlockId = "report-button-block-id"

type ReportInputElementActionId = "report-input-element-action-id"
type ReportInputBlockId = "report-input-block-id"

-- report view, plain text input block
reportInputElementActionId :: ActionId
reportInputElementActionId = fromString $ symbolVal $ Proxy @ReportInputElementActionId

reportInputBlockId :: BlockId
reportInputBlockId = fromString $ symbolVal $ Proxy @ReportInputBlockId

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
