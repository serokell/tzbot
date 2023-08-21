-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Slack.API.Common where

import TzPrelude

import Data.Aeson

import TzBot.Instances ()
import TzBot.Util

-- | See https://api.slack.com/reference/block-kit/composition-objects#text
newtype PlainText = PlainText { ptText :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype IsString
  deriving ToJSON via TypedWrapper PlainText

-- TODO: Add type-level explicit text formatting
-- | See https://api.slack.com/reference/block-kit/composition-objects#text
newtype Mrkdwn = Mrkdwn { mtText :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype IsString
  deriving ToJSON via TypedWrapper Mrkdwn

{-
>>> encode $ Mrkdwn "asd"
"{\"text\":\"asd\",\"type\":\"mrkdwn\"}"
 -}

data TextObject
  = TOPlainText PlainText
  | TOMarkdownText Mrkdwn
  deriving stock (Eq, Show, Generic)
  deriving ToJSON via SumWrapper TextObject
