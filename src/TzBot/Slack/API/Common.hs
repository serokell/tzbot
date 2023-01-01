-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Slack.API.Common where

import Universum

import Data.Aeson

import TzBot.Instances ()
import TzBot.Util

-- | See https://api.slack.com/reference/block-kit/composition-objects#text
newtype PlainText = PlainText { ptText :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype IsString
  deriving ToJSON via TypedWrapper PlainText

{-
>>> encode $ PlainText "hello"
"{\"text\":\"hello\",\"type\":\"plain_text\"}"
 -}
