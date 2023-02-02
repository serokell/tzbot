-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.TimeReference where

import Universum

import Control.Arrow ((>>>))
import Data.List.NonEmpty qualified as NE
import Data.Time
  (Day, DayOfWeek, LocalTime(localDay), NominalDiffTime, TimeOfDay, TimeZone, UTCTime, addLocalTime,
  diffLocalTime, localTimeToUTC, nominalDay, toGregorian, utc, utcToLocalTime)
import Data.Time.Calendar.Compat (DayOfMonth, MonthOfYear)
import Data.Time.TZInfo qualified as TZI
import Data.Time.TZTime qualified as TZT
import Data.Time.Zones.All (TZLabel)
import Formatting (Buildable)

import TzBot.Util qualified as CI

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
  | TimeZoneAbbreviationRef TimeZoneAbbreviation
  -- ^ A timezone abbreviation, e.g. @GMT@.
  deriving stock (Eq, Show)

-- | A timezone abbreviation such as @GMT@ or @EST@.
-- Usually composed of 2-5 uppercase letters.
-- See: https://en.wikipedia.org/wiki/List_of_time_zone_abbreviations
newtype TimeZoneAbbreviation = TimeZoneAbbreviation { unTimeZoneAbbreviation :: Text }
  deriving newtype (Eq, Show, IsString, Buildable)

-- | An offset from UTC measured in minutes.
newtype Offset = Offset { unOffset :: Int }
  deriving newtype (Eq, Show, Num)

secondsPerMinute :: Int
secondsPerMinute = 60

offsetToNominalDiffTime :: Offset -> NominalDiffTime
offsetToNominalDiffTime (Offset minutes) =
  fromIntegral @Int @NominalDiffTime (minutes * secondsPerMinute)

utcToUtcLocalTime :: UTCTime -> LocalTime
utcToUtcLocalTime = utcToLocalTime utc

utcLocalTimeToUTC :: LocalTime -> UTCTime
utcLocalTimeToUTC = localTimeToUTC utc

convertUtcToOffsetTime :: Offset -> UTCTime -> LocalTime
convertUtcToOffsetTime offset utcTime =
  addLocalTime (offsetToNominalDiffTime offset) (utcToUtcLocalTime utcTime)

convertOffsetTimeToUtc :: Offset -> LocalTime -> UTCTime
convertOffsetTimeToUtc offset localTime =
  utcLocalTimeToUTC $ addLocalTime (negate $ offsetToNominalDiffTime offset) localTime

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
timeReferenceToUTC sendersTZLabel eventTimestamp TimeReference {..} =
  case mbEitherTzOrOffset of
    Left abbrev -> TRTUInvalidTimeZoneAbbrev abbrev
    Right (Right offset) -> do
      -- In the case of rigid offset we don't need the `modifyLocal` from
      -- from the `tztime` package because there are no timeshifts that
      -- we should take into account. So we just use plain LocalTime.
      let refTime = eventTimestamp & (
            convertUtcToOffsetTime offset
            >>> dayTransition
            >>> TZT.atTimeOfDay trTimeOfDay
            )
      TRTUSuccess $ TimeRefSuccess
        (convertOffsetTimeToUtc offset refTime)
        (Right offset)
        (localDay refTime)
    Right (Left (tzLabel, implicitSenderTimezone)) -> do
      let eventLocalTime = TZT.fromUTC (TZI.fromLabel tzLabel) eventTimestamp
      let eithRefTime = eventLocalTime & TZT.modifyLocalStrict (
            dayTransition >>> TZT.atTimeOfDay trTimeOfDay
            )
      case eithRefTime of
        Left err -> tzErrorToResult implicitSenderTimezone tzLabel err
        Right refTime -> TRTUSuccess $ TimeRefSuccess
          (TZT.toUTC refTime)
          (Left tzLabel)
          (localDay $ TZT.tzTimeLocalTime refTime)

  where
  tzErrorToResult :: Bool -> TZLabel -> TZT.TZError -> TimeReferenceToUTCResult
  tzErrorToResult implicitSenderTimezone tzLabel = \case
    TZT.TZOverlap invalidTime _ _ -> TRTUAmbiguous $
      TimeShiftErrorInfo implicitSenderTimezone tzLabel (localDay invalidTime)
    TZT.TZGap invalidTime _ _ -> TRTUInvalid $
      TimeShiftErrorInfo implicitSenderTimezone tzLabel (localDay invalidTime)

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

  -- The outer `Either` acts like an error carrier, so for it we use `pure` and `<$>`,
  -- and the inner `Either` carries one of the possible equitable results, so
  -- for it we use `Right` or `Left`.
  mbEitherTzOrOffset :: Either TimeZoneAbbreviation (Either (TZLabel, Bool) Offset)
  mbEitherTzOrOffset = case trLocationRef of
    Nothing -> pure $ Left (sendersTZLabel, True)
    Just (TimeZoneRef tzLabel) -> pure $ Left (tzLabel, False)
    Just (OffsetRef offset) -> pure $ Right offset
    Just (TimeZoneAbbreviationRef abbrev) ->
      Right . tzaiOffsetMinutes <$>
          maybe (Left abbrev) pure
            (CI.lookup (unTimeZoneAbbreviation abbrev) knownTimeZoneAbbreviations)

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
  , trsEithTzOffset   :: Either TZLabel Offset
    -- ^ The timezone or offset that this TimeReference is related to.
    -- When the `TimeReference` does not explicitly mention a timezone/offset,
    -- we assume it's related to the sender's timezone.
  , trsOriginalDate :: Day
    -- ^ The day that was originally mentioned by the sender
    -- in specified or implicit sender's timezone.
  } deriving stock (Eq, Show)

data TimeShiftErrorInfo = TimeShiftErrorInfo
  { tseiIsImplicitSenderTimezone :: Bool
    -- ^ Whether the sender's timezone was taken implicitly.
  , tseiRefTimeZone :: TZLabel
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
  | TRTUInvalidTimeZoneAbbrev TimeZoneAbbreviation
  -- ^ The timezone abbreviation used is not supported / does not exist.
  deriving stock (Eq, Show)
----------------------------------------------------------------------------
-- Timezone abbreviations
----------------------------------------------------------------------------

data TimeZoneAbbreviationInfo = TimeZoneAbbreviationInfo
  { tzaiAbbreviation :: TimeZoneAbbreviation
  , tzaiOffsetMinutes :: Offset -- ^ Offset from UTC in minutes.
  , tzaiFullName :: Text
  }

knownTimeZoneAbbreviations :: CI.CIStorage TimeZoneAbbreviationInfo
knownTimeZoneAbbreviations =
  CI.fromList . map (\tzAbbr ->
    (unTimeZoneAbbreviation $ tzaiAbbreviation tzAbbr, tzAbbr)) $
      knownTimeZoneAbbreviations'

-- | A subset of https://en.wikipedia.org/wiki/List_of_time_zone_abbreviations
knownTimeZoneAbbreviations' :: [TimeZoneAbbreviationInfo]
knownTimeZoneAbbreviations' =
  -- TODO: add more tz abbreviations.
  --
  -- NOTE: Remember to update `docs/timezone_abbreviations.md` when making changes to this list.
  [ TimeZoneAbbreviationInfo "UTC" 0 "UTC"
  , TimeZoneAbbreviationInfo "GMT" 0 "GMT"
  , TimeZoneAbbreviationInfo "HST"   (hours -10)       "Hawaii-Aleutian Standard Time"
  , TimeZoneAbbreviationInfo "HDT"   (hours -09)       "Hawaii-Aleutian Daylight Time"
  , TimeZoneAbbreviationInfo "PST"   (hours -08)       "Pacific Standard Time (North America)"
  , TimeZoneAbbreviationInfo "PDT"   (hours -07)       "Pacific Daylight Time (North America)"
  , TimeZoneAbbreviationInfo "MST"   (hours -07)       "Mountain Standard Time (North America)"
  , TimeZoneAbbreviationInfo "MDT"   (hours -06)       "Mountain Daylight Time (North America)"
  , TimeZoneAbbreviationInfo "CST"   (hours -06)       "Central Standard Time (North America)"
  , TimeZoneAbbreviationInfo "CDT"   (hours -05)       "Central Daylight Time (North America)"
  , TimeZoneAbbreviationInfo "EST"   (hours -05)       "Eastern Standard Time (North America)"
  , TimeZoneAbbreviationInfo "EDT"   (hours -04)       "Eastern Daylight Time (North America)"
  , TimeZoneAbbreviationInfo "AST"   (hours -04)       "Atlantic Standard Time"
  , TimeZoneAbbreviationInfo "ADT"   (hours -03)       "Atlantic Daylight Time"
  , TimeZoneAbbreviationInfo "AMT"   (hours -04)       "Amazon Time"
  , TimeZoneAbbreviationInfo "AMST"  (hours -03)       "Amazon Summer Time"
  , TimeZoneAbbreviationInfo "CLT"   (hours -04)       "Chile Standard Time"
  , TimeZoneAbbreviationInfo "CLST"  (hours -03)       "Chile Summer Time"
  , TimeZoneAbbreviationInfo "BRT"   (hours -03)       "Brasilia Time"
  , TimeZoneAbbreviationInfo "BRST"  (hours -02)       "Brasilia Summer Time"
  , TimeZoneAbbreviationInfo "WET"   (hours  00)       "Western European Time"
  , TimeZoneAbbreviationInfo "WEST"  (hours  01)       "Western European Summer Time"
  , TimeZoneAbbreviationInfo "BST"   (hours  01)       "British Summer Time"
  , TimeZoneAbbreviationInfo "CET"   (hours  01)       "Central European Time"
  , TimeZoneAbbreviationInfo "CEST"  (hours  02)       "Central European Summer Time"
  , TimeZoneAbbreviationInfo "WAT"   (hours  01)       "West Africa Time"
  , TimeZoneAbbreviationInfo "WAST"  (hours  02)       "West Africa Summer Time"
  , TimeZoneAbbreviationInfo "CAT"   (hours  02)       "Central Africa Time"
  , TimeZoneAbbreviationInfo "SAST"  (hours  02)       "South African Standard Time"
  , TimeZoneAbbreviationInfo "EET"   (hours  02)       "Eastern European Time"
  , TimeZoneAbbreviationInfo "EEST"  (hours  03)       "Eastern European Summer Time"
  , TimeZoneAbbreviationInfo "MSK"   (hours  03)       "Moscow Time"
  , TimeZoneAbbreviationInfo "TRT"   (hours  03)       "Turkey Time"
  , TimeZoneAbbreviationInfo "GET"   (hours  04)       "Georgia Standard Time"
  , TimeZoneAbbreviationInfo "IST"   (hours  05 + 30)  "India Standard Time"
  , TimeZoneAbbreviationInfo "AWST"  (hours  08)       "Australian Western Standard Time"
  , TimeZoneAbbreviationInfo "AWDT"  (hours  09)       "Australian Western Daylight Time"
  , TimeZoneAbbreviationInfo "ACWST" (hours  08 + 45)  "Australian Central Western Standard Time"
  , TimeZoneAbbreviationInfo "JST"   (hours  09)       "Japan Standard Time"
  , TimeZoneAbbreviationInfo "KST"   (hours  09)       "Korea Standard Time"
  , TimeZoneAbbreviationInfo "ACST"  (hours  09 + 30)  "Australian Central Standard Time"
  , TimeZoneAbbreviationInfo "ACDT"  (hours  10 + 30)  "Australian Central Daylight Time"
  , TimeZoneAbbreviationInfo "AEST"  (hours  10)       "Australian Eastern Standard Time"
  , TimeZoneAbbreviationInfo "AEDT"  (hours  11)       "Australian Eastern Daylight Time"
  , TimeZoneAbbreviationInfo "NZST"  (hours  12)       "New Zealand Standard Time"
  , TimeZoneAbbreviationInfo "NZDT"  (hours  13)       "New Zealand Daylight Time"
  ]
  where
    hours :: Int -> Offset
    hours h = Offset $ h * 60
