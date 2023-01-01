-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvents.BlockAction (
  BlockAction(..),
  getBlockAction,
  processReportButtonToggled,
  processReportButtonFromEphemeral) where

import Universum

import Data.Aeson
import Text.Interpolation.Nyan

import TzBot.Feedback.Dialog
import TzBot.Feedback.Dialog.Types
import TzBot.ProcessEvents.Common (openModalCommon)
import TzBot.RunMonad (log')
import TzBot.Slack
import TzBot.Slack.API
import TzBot.Slack.Events
import TzBot.Slack.Fixtures qualified as Fixtures
import TzBot.Slack.Modal
import TzBot.Util

data BlockAction =
  BAReportButtonToggled ViewActionEvent
  | BAReportButtonFromEphemeral (ReportEphemeralEvent, Fixtures.MessageIdentifier)

getBlockAction :: Text -> Value -> Maybe BlockAction
getBlockAction actionId val =
  asum
    [ BAReportButtonToggled <$> getReportButtonToggled actionId val
    , BAReportButtonFromEphemeral <$> getReportButtonFromEphemeral actionId val
    ]

getReportButtonToggled :: Text -> Value -> Maybe ViewActionEvent
getReportButtonToggled actionId val = do
  guard (actionId == unActionId Fixtures.reportButtonActionId)
  decodeMaybe val

getReportButtonFromEphemeral :: Text -> Value -> Maybe (ReportEphemeralEvent, Fixtures.MessageIdentifier)
getReportButtonFromEphemeral actionId val = do
  messageId <- Fixtures.getMessageIdFromReportEphemeral $ ActionId actionId
  res <- decodeMaybe val
  pure (res, messageId)

processReportButtonToggled :: ViewActionEvent -> BotM ()
processReportButtonToggled val = do
  let metadataEntryId = vPrivateMetadata $ vaeView val
  mbMetadata <- lookupDialogEntry metadataEntryId
  case mbMetadata of
    Nothing -> log' [int||Dialog id not found: #{metadataEntryId}|]
    Just _metadata@ReportDialogEntry {..} -> updateModal $
      UpdateViewReq
        (mkReportModal rpmMessageText rpmTimeTranslation metadataEntryId)
        (vId $ vaeView val)

processReportButtonFromEphemeral :: (ReportEphemeralEvent, Fixtures.MessageIdentifier) -> BotM ()
processReportButtonFromEphemeral (evt, Fixtures.MessageIdentifier {..}) = do
  let channelId = scId $ reeChannel evt
  msg <- case miThreadId of
    Nothing -> retrieveOneMessage channelId miMessageId
    Just threadId -> retrieveOneMessageFromThread channelId threadId miMessageId
  let whoTriggeredId = suId $ reeUser evt
      triggerId = reeTriggerId evt
  openModalCommon msg whoTriggeredId triggerId mkReportModal
