-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Render (
  -- * Types
  TranslationPair(..)
  , TranslationPairs
  , Template

  -- * Render generic
  , renderAllTP
  , renderErrorsTP
  , joinTranslationPairs
  , joinMaybeTranslationPairs

  -- * Render text
  , renderErrors
  , renderAll

  -- * Render Slack
  , renderSlackBlock

  -- * General template
  , renderTemplate
  ) where

import Universum

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Time
import Data.Time.Compat
import Data.Time.TZInfo qualified as TZI
import Data.Time.TZTime qualified as TZT
import Data.Time.Zones.All (TZLabel)
import Text.Interpolation.Nyan

import Data.Aeson
import TzBot.Instances ()
import TzBot.Slack.API (PlainText(..), User(uTz))
import TzBot.Slack.API.Block
import TzBot.TimeReference
import TzBot.Util

-- Types
type EitherTemplateUnit = Either TranslationPair (User -> TranslationPair)

data TranslationPair = TranslationPair
  { tuTimeRef :: Text
  , tuTranslation :: Text
  } deriving stock (Eq, Show, Generic)
    deriving ToJSON via RecordWrapper TranslationPair

type TranslationPairs = NE.NonEmpty TranslationPair

newtype Template = Template { unTemplate :: NE.NonEmpty EitherTemplateUnit }

concatTranslationPair :: TranslationPair -> Text
concatTranslationPair TranslationPair {..} = mconcat [tuTimeRef, ":  ", tuTranslation]

-- Render generic
renderErrorsTP :: Template -> Maybe TranslationPairs
renderErrorsTP (Template lst) = NE.nonEmpty $ lefts $ NE.toList lst

renderAllTP :: User -> Template -> TranslationPairs
renderAllTP user = NE.map (either id ($ user)) . unTemplate

joinTranslationPairs :: TranslationPairs -> Text
joinTranslationPairs = T.unlines . NE.toList . NE.map concatTranslationPair

joinMaybeTranslationPairs :: Maybe TranslationPairs -> Text
joinMaybeTranslationPairs = maybe noRefsFoundMsg joinTranslationPairs

-- | We show this message because we may have not found
-- any time references while actually there were.
noRefsFoundMsg :: Text
noRefsFoundMsg = "No time references found."

-- Render text
renderErrors :: Template -> Maybe Text
renderErrors template = joinTranslationPairs <$> renderErrorsTP template

renderAll :: User -> Template -> Text
renderAll user = joinTranslationPairs . renderAllTP user

-- Render Slack block
renderSlackBlock :: Maybe TranslationPairs -> Block
renderSlackBlock = maybe noRefsFoundSection mkFieldsSection
  where
    noRefsFoundSection = BSection $ textSection (PlainText noRefsFoundMsg) Nothing
    mkFieldsSection translationPairs =
      BSection $ fieldsSection Nothing Nothing
        $ flip map translationPairs $ \timeRef ->
          (PlainText $ tuTimeRef timeRef, PlainText $ tuTranslation timeRef)

renderTemplate :: UTCTime -> User -> NE.NonEmpty TimeReference -> Template
renderTemplate now sender timeRefs =
  Template $ NE.map (renderEphemeralMessageTranslationPair now sender)
    $ attach (timeReferenceToUTC (uTz sender) now) timeRefs

-- Under `Left` we collect errors, that should be shown to all users (including sender),
-- and under `Right` we collect valid time references that should be rendered differently
-- for each user.
renderEphemeralMessageTranslationPair
  :: UTCTime
  -> User
  -> (TimeReference, TimeReferenceToUTCResult)
  -> EitherTemplateUnit
renderEphemeralMessageTranslationPair now sender (timeRef, result) = case result of
  TRTUSuccess utcTime _offsetInfo -> do
    let mbSenderTimeZone =
          guard (isNothing $ trLocationRef timeRef)
            $> (uTz sender) :: Maybe TZLabel
        mbSenderTimeZoneAux = mbSenderTimeZone $> " in " :: Maybe Text
    Right $ \user -> do
      let userTzLabel = uTz user
          renderedUserTime = renderUserTime userTzLabel now utcTime
      TranslationPair
        [int||"#{trText timeRef}"#{mbSenderTimeZoneAux}#{mbSenderTimeZone}|]
        [int||#{renderedUserTime} in #{userTzLabel}|]
  TRTUAmbiguous ->
    Left $ TranslationPair (trText timeRef) "ambiguous because of the time shift"
  TRTUInvalid ->
    Left $ TranslationPair (trText timeRef) "invalid because of the time shift"
  TRTUInvalidTimeZoneAbbrev abbrev ->
    Left $ TranslationPair (trText timeRef)
      [int||contains invalid timezone abbreviation: #{abbrev}|]

renderUserTime :: TZLabel -> UTCTime -> UTCTime -> String
renderUserTime tzLabel now refTime = do
  let tzInfo = TZI.fromLabel tzLabel
      currentUserLocalTime = TZT.tzTimeLocalTime $ TZT.fromUTC tzInfo now
      refUserLocalTime = TZT.tzTimeLocalTime $ TZT.fromUTC tzInfo refTime
      sameYear = theYearIsTheSame currentUserLocalTime refUserLocalTime
      yearFormat = if sameYear then "" else ", %Y"
      -- example:
      -- 18:23, Monday, December 26, 2016
      format = "%H:%M, %A, %B %d" <> yearFormat
  formatTime defaultTimeLocale format refUserLocalTime

theYearIsTheSame :: LocalTime -> LocalTime -> Bool
theYearIsTheSame t1 t2 = do
  let YearMonthDay y1 _ _ = localDay t1
  let YearMonthDay y2 _ _ = localDay t2
  y1 == y2
