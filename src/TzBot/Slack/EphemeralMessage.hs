-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Slack.EphemeralMessage where

import Universum

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Time
import Data.Time.Compat
import Data.Time.TZInfo qualified as TZI
import Data.Time.TZTime qualified as TZT
import Data.Time.Zones.All (TZLabel)
import Text.Interpolation.Nyan

import TzBot.Instances ()
import TzBot.Slack.API (User(uTz))
import TzBot.TimeReference

renderErrorsForSender :: NE.NonEmpty (Either Text (User -> Text)) -> Maybe Text
renderErrorsForSender lst = do
  let errors = lefts $ NE.toList lst
  guard $ isJust $ NE.nonEmpty errors
  pure $ T.unlines errors

renderEphemeralMessageForOthers :: User -> NE.NonEmpty (Either Text (User -> Text)) -> Text
renderEphemeralMessageForOthers user = T.unlines . NE.toList . NE.map (either id ($ user))

-- Under `Left` we collect errors, that should be shown to all users (including sender),
-- and under `Right` we collect valid time references that should be rendered differently
-- for each user.
renderEphemeralMessageTemplate :: UTCTime -> User -> (TimeReference, TimeReferenceToUTCResult) -> Either Text (User -> Text)
renderEphemeralMessageTemplate now sender (timeRef, result) = case result of
  TRTUSuccess utcTime _offsetInfo -> do
    let mbSenderTimeZone = guard (isNothing $ trLocationRef timeRef) $> (uTz sender) :: Maybe TZLabel
        mbSenderTimeZoneAux = mbSenderTimeZone $> " in " :: Maybe Text
    Right $ \user -> do
      let userTzLabel = uTz user
          renderedUserTime = renderUserTime userTzLabel now utcTime
      [int||"#{trText timeRef}"#{mbSenderTimeZoneAux}#{mbSenderTimeZone} is #{renderedUserTime} in #{userTzLabel}|]
  TRTUAmbiguous ->
    Left [int||"#{trText timeRef}" is ambiguous because of the time shift|]
  TRTUInvalid ->
    Left [int||"#{trText timeRef}" is invalid because of the time shift|]
  TRTUInvalidTimeZoneAbbrev abbrev ->
    Left [int||"#{trText timeRef}" contains invalid timezone abbreviation: #{abbrev}|]

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
