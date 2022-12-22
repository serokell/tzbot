-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_GHC -Wno-orphans #-}

-- | A module with orphan instances.
module TzBot.Instances where

import Universum

import Data.Aeson
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time.Zones.All (TZLabel)
import Data.Time.Zones.All qualified as TZ

instance FromJSON TZLabel where
  parseJSON =
    withText "timezone" \t ->
      case TZ.fromTZName (T.encodeUtf8 t) of
        Just tzLabel -> pure tzLabel
        Nothing -> fail $ "Invalid timezone: '" <> T.unpack t <> "'"
