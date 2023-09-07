-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Slack.Modal where

import TzPrelude

import Data.List (singleton)
import Text.Interpolation.Nyan

import TzBot.Feedback.Dialog.Types
import TzBot.Instances ()
import TzBot.Render (ConversionPairs, asForOthersS, renderSlackBlocks)
import TzBot.Slack.API
import TzBot.Slack.Fixtures qualified as Fixtures

-- View modal
mkShowModal :: Text -> Maybe ConversionPairs -> ReportDialogId -> Modal
mkShowModal shownMessageText conversionPairsMb metadata =
  Modal
    { mTitle = "View time references"
    , mSubmit = Nothing
    , mNotifyOnClose = False
    , mCallbackId = Fixtures.viewModal
    , mPrivateMetadata = unReportDialogId metadata
    , mBlocks = mkBlocks shownMessageText conversionPairsMb $ BActions reportButton
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
mkReportModal :: Text -> Maybe ConversionPairs -> ReportDialogId -> Modal
mkReportModal shownMessageText conversionPairsMb metadata =
  Modal
    { mTitle = "Report"
    , mSubmit = Just "Submit"
    , mNotifyOnClose = False
    , mCallbackId = Fixtures.reportModal
    , mPrivateMetadata = unReportDialogId metadata
    , mBlocks = mkBlocks messageText conversionPairsMb $ BInput reportInput
    }
  where
    messageText =
      [int||
        *Note*: The contents of this message will be shared with the development team.


        #{shownMessageText}
      |]

reportInput :: Input
reportInput = Input
      { iLabel = "Are there any issues with how this message was processed?"
      , iBlockId = Fixtures.reportInputBlockId
      , iElement = PlainTextInput
        { ptiActionId = Fixtures.reportInputElementActionId
        , ptiMultiline = True
        , ptiFocusOnLoad = True
        , ptiPlaceholder = "Type your thoughts here"
        }
      }

-- Common funcs
mkBlocks :: Text -> Maybe ConversionPairs -> Block -> [Block]
mkBlocks shownMessageText conversionPairsMb block =
  -- We always render the conversion for other users (not author),
  -- so the author can see how their message is converted for others
  [ BHeader Header { hText = PlainText "Message text" }
  , BSection $ markdownSection (Mrkdwn shownMessageText)
  , BDivider divider
  , BHeader Header { hText = PlainText "Time references" }
  ]
  <> renderSlackBlocks asForOthersS conversionPairsMb
  <> [ BDivider divider
  , block
  ]
