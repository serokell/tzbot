-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_GHC -Wno-orphans #-}

-- | A module with orphan instances.
module TzBot.Instances () where

import Universum

import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), Value(String), withText)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time.Zones.All (TZLabel, toTZName)
import Data.Time.Zones.All qualified as TZ
import Formatting.Buildable (Buildable(..))
import Time (KnownRatName, Time, unitsF, unitsP)

instance Buildable TZLabel where
  build = build . T.decodeUtf8 . toTZName

instance FromJSON TZLabel where
  parseJSON =
    withText "timezone" \t ->
      case TZ.fromTZName (T.encodeUtf8 t) of
        Just tzLabel -> pure tzLabel
        Nothing -> fail $ "Invalid timezone: '" <> T.unpack t <> "'"

instance KnownRatName unit => FromJSON (Time unit) where
  parseJSON = withText "Time string" $ \t -> case unitsP @unit (toString t) of
    Just x  -> pure x
    Nothing -> fail "Invalid time"

instance KnownRatName unit => ToJSON (Time unit) where
  toJSON = String . fromString . unitsF
