-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module TzBot.Orphans where

import Universum

import Data.Text.Encoding qualified as T
import Data.Time.Zones.All (TZLabel, toTZName)
import Formatting.Buildable

instance Buildable TZLabel where
  build = build . T.decodeUtf8 . toTZName
