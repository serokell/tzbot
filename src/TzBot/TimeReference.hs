-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.TimeReference where

import Universum

import Control.Arrow ((>>>))
import Data.List.NonEmpty qualified as NE
import Data.Time
  (Day, DayOfWeek, LocalTime(localDay), TimeOfDay, TimeZone, UTCTime, diffLocalTime, nominalDay,
  toGregorian)
import Data.Time.Calendar.Compat (DayOfMonth, MonthOfYear)
import Data.Time.TZInfo qualified as TZI
import Data.Time.TZTime qualified as TZT
import Data.Time.Zones.All (TZLabel)
import Data.Time.Zones.Types (TZ(..))
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Formatting (Buildable)
import Formatting.Buildable (Buildable(..))
import Text.Interpolation.Nyan (int, rmode')
import Text.Printf (printf)

{- | An offset from UTC (e.g. @UTC+01:00@) with an optional timezone abbreviation (e.g. @BST@).

Note: The `TimeZone` data type from the @time@ package is a misnomer, it doesn't actually represent a timezone.

A timezone contains a set of rules dictating which offset(s) is/are observed throughout the year.
For example: under current law, the "Europe/London" timezone observes the offset BST (UTC+01:00)
during summer and the offset GMT (UTC+00:00) otherwise.
These rules change over time by governmental decree.

The `TimeZone` data type, on the other hand, only represents a single static offset.
We use this type alias to make this distinction a bit more clear.

(In fact, the @time@ package does not support timezones at all.)
-}
type NamedOffset = TimeZone

type TimeReferenceText = Text

-- | A reference to a point in time, e.g. "tuesday at 10am", "3pm CST on July 7th"
data TimeReference = TimeReference
  { trText :: TimeReferenceText -- ^ The original section of the text from where this `TimeReference` was parsed.
  , trTimeOfDay :: TimeOfDay
  , trDateRef :: Maybe DateReference
  , trLocationRef :: Maybe LocationReference
  }
  deriving stock (Eq, Show)

getTzLabelMaybe :: TZLabel -> TimeReference -> Maybe TZLabel
getTzLabelMaybe senderTz timeRef = case trLocationRef timeRef of
  Just (TimeZoneRef refTzLabel) -> Just refTzLabel
  Nothing -> Just senderTz
  _ -> Nothing

data DateReference
  = DaysFromToday Int
  | DayOfWeekRef DayOfWeek
  | DayOfMonthRef DayOfMonth (Maybe MonthOfYear)
  deriving stock (Eq, Show)

data LocationReference
  = TimeZoneRef TZLabel
  -- ^ A timezone name, e.g. @Europe/London@.
  | OffsetRef Offset
  -- ^ An offset from UTC, e.g. @UTC+03:00@
  | TimeZoneAbbreviationRef TimeZoneAbbreviationInfo
  -- ^ A timezone abbreviation, e.g. @GMT@.
  | UnknownTimeZoneAbbreviationRef UnknownTimeZoneAbbrev
  -- ^ An unknown timezone abbreviation, not listed in the
  -- `TzBot.Parser.knownTimeZoneAbbreviations` storage
  deriving stock (Eq, Show)

-- | A timezone abbreviation such as @GMT@ or @EST@.
-- Usually composed of 2-5 uppercase letters.
-- See: https://en.wikipedia.org/wiki/List_of_time_zone_abbreviations
newtype TimeZoneAbbreviation = TimeZoneAbbreviation { unTimeZoneAbbreviation :: Text }
  deriving newtype (Eq, Show, IsString, Buildable)

-- | A timezone abbreviation that we do not recognize.
data UnknownTimeZoneAbbrev = UnknownTimeZoneAbbrev
  { utzaAbbrev :: TimeZoneAbbreviation
    -- ^ The unrecognized abbreviation, possibly but not necessarily due to a typo, e.g. @GTM@.
  , utzaCandidates :: [TimeZoneAbbreviation]
    -- ^ Assuming the user made a typo, this is a list of supported
    -- abbreviations that are similar enough to what they wrote, e.g. @GMT@.
  } deriving stock (Eq, Show)

-- | An offset from UTC measured in minutes.
newtype Offset = Offset { unOffset :: Int }
  deriving newtype (Eq, Show, Num)

instance Buildable Offset where
  build = fromString . renderOffset

renderOffset :: Offset -> String
renderOffset (Offset minutesOffset) = do
  let sign = if minutesOffset >= 0 then "+" else "-" :: String
      minutesPerHour = 60
      (hours, mins) = abs minutesOffset `divMod` minutesPerHour
  printf ("UTC" <> sign <> "%02d:%02d") hours mins

secondsPerMinute :: Int
secondsPerMinute = 60

{- | Converts a time reference to a moment in time (expressed in UTC).

  If the time reference contains a timezone abbreviation, and if that abbreviation\
  is invalid or not supported, this returns an error.

  If the timezone abbreviation is valid and supported, we assume that
  the user prefers to use it as a reference timezone, so we ignore
  their own timezone from their profile settings.
-}
timeReferenceToUTC
  :: TZLabel -- ^ The timezone of the sender of the Slack message.
  -> UTCTime -- ^ The time at which the message was sent.
  -> TimeReference -- ^ A time reference to translate to UTC.
  -> TimeReferenceToUTCResult
timeReferenceToUTC sendersTZLabel eventTimestamp timeRef@TimeReference {..} =
  case eithTzInfo of
    Left abbrev -> TRTUInvalidTimeZoneAbbrev abbrev
    Right tzInfo -> do
      let eventLocalTime = TZT.fromUTC tzInfo eventTimestamp
      let eithRefTime = eventLocalTime & TZT.modifyLocalStrict (
            dayTransition >>> TZT.atTimeOfDay trTimeOfDay
            )
      case eithRefTime of
        Left err -> do
          case getTzLabelMaybe sendersTZLabel timeRef of
            Nothing -> error "impossible happened: got TZError for a static offset"
            Just refTzLabel -> tzErrorToResult refTzLabel err
        Right refTime -> TRTUSuccess $ TimeRefSuccess
          (TZT.toUTC refTime)
          (localDay $ TZT.tzTimeLocalTime refTime)

  where
  tzErrorToResult :: TZLabel -> TZT.TZError -> TimeReferenceToUTCResult
  tzErrorToResult tzLabel = \case
    TZT.TZOverlap invalidTime _ _ -> TRTUAmbiguous $
      TimeShiftErrorInfo tzLabel (localDay invalidTime)
    TZT.TZGap invalidTime _ _ -> TRTUInvalid $
      TimeShiftErrorInfo tzLabel (localDay invalidTime)

  -- This doesn't include setting time, only date changes
  dayTransition :: LocalTime -> LocalTime
  dayTransition eventLocalTime = case trDateRef of
    Nothing -> do
      let thatTimeOfCurrentDay = TZT.atTimeOfDay trTimeOfDay eventLocalTime
      if thatTimeOfCurrentDay >= eventLocalTime
        then eventLocalTime
        else TZT.addCalendarClip (TZT.calendarDays 1) eventLocalTime
    Just (DaysFromToday n) ->
      TZT.addCalendarClip (TZT.calendarDays $ fromIntegral @Int @Integer n) eventLocalTime
    Just (DayOfWeekRef dayOfWeek) ->
      TZT.atFirstDayOfWeekOnAfter dayOfWeek eventLocalTime
    Just (DayOfMonthRef dayOfMonth mbMonthOfYear) -> case mbMonthOfYear of
      Nothing -> chooseBestMonth dayOfMonth eventLocalTime
      Just monthOfYear -> chooseBestYear dayOfMonth monthOfYear eventLocalTime

  eithTzInfo :: Either UnknownTimeZoneAbbrev TZI.TZInfo
  eithTzInfo = case trLocationRef of
    Nothing -> pure $ TZI.fromLabel sendersTZLabel
    Just (TimeZoneRef tzLabel) -> pure $ TZI.fromLabel tzLabel
    Just (OffsetRef offset) -> pure $ tzInfoFromOffset offset
    Just (TimeZoneAbbreviationRef abbrev) -> pure $ tzInfoFromOffset $ tzaiOffsetMinutes abbrev
    Just (UnknownTimeZoneAbbreviationRef unknownAbbrev) -> Left unknownAbbrev

  tzInfoFromOffset :: Offset -> TZI.TZInfo
  tzInfoFromOffset (Offset offsetMinutes) =
    TZI.TZInfo shownOffset $ TZ
        (VU.singleton minBound)
        (VU.singleton $ secondsPerMinute * offsetMinutes)
        (V.singleton (False, toString shownOffset))
    where
      shownOffset = [int||#{offsetMinutes}|] :: Text

-- | Given a day of month and current time, try to figure out what day was really meant.
-- Algorithm:
--
-- 1. Take current month and its neighbors and take the given day of month
--    for that months (clip if there's no such day in this month);
-- 2. Calculate absolute difference between that days and current day,
--    obtaining some scores;
-- 3. Our goal is to pick the day with the lowest score; but we prefer
--    future, so we add some additional score to the days in the past;
-- 4. Try to pick the date which day of month is the same as mentioned
--    by the user, with the lowest score;
-- 5. If it's not found, pick just the day with the lowest score
--    (normally this shouldn't occur, only for errors like 32th January.
chooseBestMonth :: DayOfMonth -> LocalTime -> LocalTime
chooseBestMonth dayOfMonth now = do
  let candidateDiffs = 0 :| [-1, 1]
      candidates = flip NE.map candidateDiffs $ \x ->
        TZT.atDayOfMonth dayOfMonth $ TZT.addCalendarClip (TZT.calendarMonths x) now
      preferFuture = 10 * nominalDay
      calcWeight x = let diff = diffLocalTime x now
                      in if diff > 0
                         then diff
                         else negate diff + preferFuture
      sortedCandidates = NE.sortBy (compare `on` calcWeight) candidates
      getDayOfMonth x = let (_, _, day) = toGregorian $ localDay x in day
      isDayOfMonthTheSame c = getDayOfMonth c == dayOfMonth
  case find isDayOfMonthTheSame sortedCandidates of
    Just res -> res
    Nothing -> NE.head sortedCandidates

-- | Given a day of month, a month of year and current time, try to figure out
--   what day was really meant. The algorithm is essentially the same as
--   for the `chooseBestMonth` function, except for the precise matching check,
--   i.e. if user mentions 29 February and now it's 1 January of non-leap year,
--   the result will be 28 February of that year.
chooseBestYear :: DayOfMonth -> MonthOfYear -> LocalTime -> LocalTime
chooseBestYear dayOfMonth monthOfYear now = do
  -- month first, then day
  let thatDayAndMonthOfCurrentYear = now &
        (TZT.atMonthOfYear monthOfYear >>> TZT.atDayOfMonth dayOfMonth)
      lastYear = TZT.addCalendarClip (TZT.calendarYears (-1)) thatDayAndMonthOfCurrentYear
      nextYear = TZT.addCalendarClip (TZT.calendarYears 1) thatDayAndMonthOfCurrentYear
      candidates = thatDayAndMonthOfCurrentYear :| [lastYear, nextYear]
      preferFuture = 6 * 30 * nominalDay -- 6 months
      calcWeight x = let diff = diffLocalTime x now
                      in if diff > 0
                         then diff
                         else negate diff + preferFuture
      sortedCandidates = NE.sortBy (compare `on` calcWeight) candidates
  NE.head sortedCandidates

data TimeRefSuccess = TimeRefSuccess
  { trsUtcResult      :: UTCTime
    -- ^ The result of the conversion.
  , trsOriginalDate :: Day
    -- ^ The day that was originally mentioned by the sender
    -- in specified or implicit sender's timezone.
  } deriving stock (Eq, Show)

data TimeShiftErrorInfo = TimeShiftErrorInfo
  { tseiRefTimeZone :: TZLabel
    -- ^ Timezone label associated with original time reference.
  , tseiOriginalDate :: Day
    -- ^ The day that was originally mentioned by the sender
    -- in specified or implicit sender's timezone.
  } deriving stock (Eq, Show)

data TimeReferenceToUTCResult
  = TRTUSuccess TimeRefSuccess
    -- ^ Conversion succeeded.
  | TRTUAmbiguous TimeShiftErrorInfo
  -- ^ The time reference was ambiguous (e.g. due to a time ocurring twice in the same timezone during DST changes).
  -- See [Edge cases & pitfalls](https://github.com/serokell/tzbot/blob/main/docs/pitfalls.md#ambiguous-times).
  | TRTUInvalid TimeShiftErrorInfo
  -- ^ The time reference was invalid (e.g. due to a time being skipped in a timezone during DST changes).
  -- See [Edge cases & pitfalls](https://github.com/serokell/tzbot/blob/main/docs/pitfalls.md#invalid-times).
  | TRTUInvalidTimeZoneAbbrev UnknownTimeZoneAbbrev
  -- ^ The timezone abbreviation used is not supported / does not exist.
  deriving stock (Eq, Show)
----------------------------------------------------------------------------
-- Timezone abbreviations
----------------------------------------------------------------------------

data TimeZoneAbbreviationInfo = TimeZoneAbbreviationInfo
  { tzaiAbbreviation :: TimeZoneAbbreviation
  , tzaiOffsetMinutes :: Offset -- ^ Offset from UTC in minutes.
  , tzaiFullName :: Text
  } deriving stock (Eq, Show)
