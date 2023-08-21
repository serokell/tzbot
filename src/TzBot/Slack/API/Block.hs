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
  , markdownSection
  , fieldsSection
  , Divider(..)
  , divider
  , Header(..)
  ) where

import TzPrelude

import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty qualified as NE
import Formatting (Buildable)

import TzBot.Instances ()
import TzBot.Slack.API.Common (Mrkdwn(..), PlainText)
import TzBot.Util (SumWrapper(..), TypedWrapper(..), neConcatMap)

newtype ActionId = ActionId { unActionId :: Text }
  deriving stock (Eq, Show, Ord)
  deriving newtype (IsString, ToJSON, FromJSON, Buildable)

newtype BlockId = BlockId { unBlockId :: Text }
  deriving stock (Eq, Show, Ord)
  deriving newtype (IsString, ToJSON, FromJSON, Buildable)

-- | See https://api.slack.com/reference/block-kit/blocks
data Block
  = BContext Context
  | BInput Input
  | BActions Actions
  | BSection Section
  | BDivider Divider
  | BHeader Header
  deriving stock (Eq, Show, Generic)
  deriving ToJSON via SumWrapper Block

-- | See https://api.slack.com/reference/block-kit/blocks#section
data Section = Section
  { sText :: Maybe Mrkdwn
  , sAccessory :: Maybe Button
  , sFields :: Maybe (NE.NonEmpty Mrkdwn)
  } deriving stock (Eq, Show, Generic)
    deriving ToJSON via TypedWrapper Section

markdownSection :: Mrkdwn -> Section
markdownSection markdown = Section (Just markdown) Nothing Nothing

fieldsSection :: Maybe Mrkdwn -> NE.NonEmpty (Mrkdwn, Mrkdwn) -> Section
fieldsSection mbText fields =
  Section mbText Nothing $ Just $ neConcatMap (\(x, y) -> x :| [y]) fields

-- | See https://api.slack.com/reference/block-kit/blocks#divider
data Divider = Divider
  { dBlockId :: Maybe BlockId
  } deriving stock (Eq, Show, Generic)
    deriving ToJSON via TypedWrapper Divider

divider :: Divider
divider = Divider Nothing

{-
>>> encode $ BDivider divider
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

-- | See https://api.slack.com/reference/block-kit/block-elements#input
data PlainTextInput = PlainTextInput
  { ptiActionId :: ActionId
  , ptiMultiline :: Bool
  , ptiFocusOnLoad :: Bool
  , ptiPlaceholder :: PlainText
  } deriving stock (Eq, Show, Generic)
    deriving ToJSON via TypedWrapper PlainTextInput
