-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Slack.API.Block
  ( Context(..)
  , Input(..)
  , PlainTextInput(..)
  , Block(..)
  , Actions(..)
  , Button(..)
  , ActionId(..)
  , BlockId(..)
  , Section
  , textSection
  , fieldsSection
  , Divider(..)
  , divider
  , Header(..)
  ) where

import Universum

import Data.Aeson

import Data.List.NonEmpty qualified as NE
import TzBot.Instances ()
import TzBot.Slack.API.Common
import TzBot.Util

newtype ActionId = ActionId { unActionId :: Text }
  deriving stock (Eq, Show, Ord)
  deriving newtype (IsString, ToJSON, FromJSON)

newtype BlockId = BlockId { unBlockId :: Text }
  deriving stock (Eq, Show, Ord)
  deriving newtype (IsString, ToJSON, FromJSON)

-- | See https://api.slack.com/reference/block-kit/blocks
data Block =
  BContext Context
  | BInput Input
  | BActions Actions
  | BSection Section
  | BDivider Divider
  | BHeader Header
  deriving stock (Eq, Show, Generic)
  deriving ToJSON via SumWrapper Block

{-
>>> encode $ BContext $ Context ["hello", "bye"]
"{\"elements\":[{\"text\":\"hello\",\"type\":\"plain_text\"},{\"text\":\"bye\",\"type\":\"plain_text\"}],\"type\":\"context\"}"
 -}

-- | See https://api.slack.com/reference/block-kit/blocks#section
data Section = Section
  { sText :: Maybe PlainText
  , sAccessory :: Maybe Button
  , sFields :: Maybe (NE.NonEmpty PlainText)
  } deriving stock (Eq, Show, Generic)
    deriving ToJSON via TypedWrapper Section

textSection :: PlainText -> Maybe Button -> Section
textSection text mbButton = Section (Just text) mbButton Nothing

fieldsSection :: Maybe PlainText -> Maybe Button -> NE.NonEmpty (PlainText, PlainText) -> Section
fieldsSection mbText mbButton fields =
  Section mbText mbButton $ Just $ neConcatMap (\(x, y) -> x :| [y]) fields

-- | See https://api.slack.com/reference/block-kit/blocks#divider
data Divider = Divider
  { dBlockId :: Maybe BlockId
  } deriving stock (Eq, Show, Generic)
    deriving ToJSON via TypedWrapper Divider

divider :: Divider
divider = Divider Nothing

{-
>>> encode $ BDivider $ Divider Nothing
"{\"type\":\"divider\"}"
 -}

-- | See https://api.slack.com/reference/block-kit/blocks#header
data Header = Header
  { hText :: PlainText
  } deriving stock (Eq, Show, Generic)
    deriving ToJSON via TypedWrapper Header

-- | See https://api.slack.com/reference/block-kit/blocks#context
data Context = Context
  { cElements :: [PlainText]
  } deriving stock (Eq, Show, Generic)
    deriving ToJSON via TypedWrapper Context
{-
>>> encode $ Context ["hello", "bye"]
"{\"elements\":[{\"text\":\"hello\",\"type\":\"plain_text\"},{\"text\":\"bye\",\"type\":\"plain_text\"}],\"type\":\"context\"}"
-}

-- | See https://api.slack.com/reference/block-kit/blocks#actions
data Actions = Actions
  { aElements :: [Button]
  , aBlockId :: BlockId
  } deriving stock (Eq, Show, Generic)
    deriving ToJSON via TypedWrapper Actions

-- | See https://api.slack.com/reference/block-kit/block-elements#button
data Button = Button
  { bText :: PlainText
  , bActionId :: ActionId
  } deriving stock (Eq, Show, Generic)
    deriving ToJSON via TypedWrapper Button

-- | See https://api.slack.com/reference/block-kit/blocks#input
data Input = Input
  { iLabel :: PlainText
  , iBlockId :: BlockId
  , iElement :: PlainTextInput
  } deriving stock (Eq, Show, Generic)
    deriving ToJSON via TypedWrapper Input

{-
>>> encode $ Input "Label" "Hint" $ PlainTextInput "plain_input_action_id" True True "Report"
"{\"element\":{\"action_id\":\"plain_input_action_id\",\"focus_on_load\":true,\"multiline\":true,\"placeholder\":{\"text\":\"Report\",\"type\":\"plain_text\"},\"type\":\"plain_text_input\"},\"hint\":{\"text\":\"Hint\",\"type\":\"plain_text\"},\"label\":{\"text\":\"Label\",\"type\":\"plain_text\"},\"type\":\"input\"}"
 -}

-- | See https://api.slack.com/reference/block-kit/block-elements#input
data PlainTextInput = PlainTextInput
  { ptiActionId :: ActionId
  , ptiMultiline :: Bool
  , ptiFocusOnLoad :: Bool
  , ptiPlaceholder :: PlainText
  } deriving stock (Eq, Show, Generic)
    deriving ToJSON via TypedWrapper PlainTextInput
{-
>>> encode $ PlainTextInput "plain_input_action_id" True True "Report"
"{\"action_id\":\"plain_input_action_id\",\"focus_on_load\":true,\"multiline\":true,\"placeholder\":{\"text\":\"Report\",\"type\":\"plain_text\"},\"type\":\"plain_text_input\"}"
-}
