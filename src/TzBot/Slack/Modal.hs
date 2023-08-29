-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Slack.Modal where

import TzPrelude

import Data.List (singleton)

import TzBot.Feedback.Dialog.Types
import TzBot.Instances ()
import TzBot.Render (TranslationPairs, asForOthersS, renderSlackBlocks)
import TzBot.Slack.API
import TzBot.Slack.Fixtures qualified as Fixtures

-- View modal
mkShowModal :: Text -> Maybe TranslationPairs -> ReportDialogId -> Modal
mkShowModal shownMessageText translatedMessage metadata =
  Modal
    { mTitle = "View time references"
    , mSubmit = Nothing
    , mNotifyOnClose = False
    , mCallbackId = Fixtures.viewModal
    , mPrivateMetadata = unReportDialogId metadata
    , mBlocks = mkBlocks shownMessageText translatedMessage $ BActions reportButton
    }

reportButton :: Actions
reportButton = Actions
  { aBlockId = Fixtures.reportButtonBlockId
  , aElements = singleton $ Button
    { bText = "Report an error"
    , bActionId = Fixtures.reportButtonActionId
    }
  }

-- Report modal
mkReportModal :: Text -> Maybe TranslationPairs -> ReportDialogId -> Modal
mkReportModal shownMessageText translatedMessage metadata =
  Modal
    { mTitle = "Report"
    , mSubmit = Just "Submit"
    , mNotifyOnClose = False
    , mCallbackId = Fixtures.reportModal
    , mPrivateMetadata = unReportDialogId metadata
    , mBlocks = mkBlocks shownMessageText translatedMessage $ BInput reportInput
    }

reportInput :: Input
reportInput = Input
      { iLabel = "Time references processed wrongly/unrecognized?"
      , iBlockId = Fixtures.reportInputBlockId
      , iElement = PlainTextInput
        { ptiActionId = Fixtures.reportInputElementActionId
        , ptiMultiline = True
        , ptiFocusOnLoad = True
        , ptiPlaceholder = "Type your thoughts here"
        }
      }

-- Common funcs
mkBlocks :: Text -> Maybe TranslationPairs -> Block -> [Block]
mkBlocks shownMessageText translatedMessage block =
  -- We always render the translation for other users (not author),
  -- so the author can see how their message is translated for others
  [ BHeader Header { hText = PlainText "Message text" }
  , BSection $ markdownSection (Mrkdwn shownMessageText)
  , BDivider divider
  , BHeader Header { hText = PlainText "Time references" }
  ]
  <> renderSlackBlocks asForOthersS translatedMessage
  <> [ BDivider divider
  , block
  ]
