-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.TimeReference where

import Universum

import Data.Maybe (fromJust)
import Data.Time (DayOfWeek, TimeOfDay, TimeZone, UTCTime)
import Data.Time.Calendar.Compat (DayOfMonth, MonthOfYear)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Time.Zones.All (TZLabel)
import Formatting (Buildable)

{- | An offset from UTC (e.g. @UTC+01:00@) with an optional timezone abbreviation (e.g. @BST@).

Note: The `TimeZone` data type from the @time@ package is a misnomer, it doesn't actually represent a timezone.

A timezone contains a set of rules dictating which offset(s) is/are observed throughout the year.
For example: under current law, the “Europe/London” timezone observes the offset BST (UTC+01:00)
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

-- | Converts a time reference to a moment in time (expressed in UTC).
--
-- If the time reference contains a timezone abbreviation, and if that abbreviation
-- is invalid or not supported, this returns a `Left`.
timeReferenceToUTC
  :: TZLabel -- ^ The timezone of the sender of the Slack message.
  -> UTCTime -- ^ The time at which the message was sent.
  -> TimeReference -- ^ A time reference to translate to UTC.
  -> TimeReferenceToUTCResult
timeReferenceToUTC sendersTimezone _eventTimestamp _timeRef =
  -- TODO [#2]
  TRTUSuccess
    (fromJust $ iso8601ParseM @_ @UTCTime "2022-03-12T13:00:00Z")
    (Left sendersTimezone)

data TimeReferenceToUTCResult
  = TRTUSuccess
    -- ^ Conversion succeeded.
      UTCTime
      -- ^ The result of the conversion.
      (Either TZLabel Offset)
      -- ^ The timezone or offset that this TimeReference is related to.
      -- When the `TimeReference` does not explicitly mention a timezone/offset,
      -- we assume it's related to the sender's timezone.
  | TRTUAmbiguous
  -- ^ The time reference was ambiguous (e.g. due to a time ocurring twice in the same timezone during DST changes).
  -- See [Edge cases & pitfalls](https://github.com/serokell/tzbot/blob/main/docs/pitfalls.md#ambiguous-times).
  | TRTUInvalid
  -- ^ The time reference was invalid (e.g. due to a time being skipped in a timezone during DST changes).
  -- See [Edge cases & pitfalls](https://github.com/serokell/tzbot/blob/main/docs/pitfalls.md#invalid-times).
  | TRTUInvalidTimeZoneAbbrev TimeZoneAbbreviation
  -- ^ The timezone abbreviation used is not supported / does not exist.

----------------------------------------------------------------------------
-- Timezone abbreviations
----------------------------------------------------------------------------

data TimeZoneAbbreviationInfo = TimeZoneAbbreviationInfo
  { tzaiAbbreviation :: TimeZoneAbbreviation
  , tzaiOffsetMinutes :: Offset -- ^ Offset from UTC in minutes.
  , tzaiFullName :: Text
  }

-- | A subset of https://en.wikipedia.org/wiki/List_of_time_zone_abbreviations
knownTimeZoneAbbreviations :: [TimeZoneAbbreviationInfo]
knownTimeZoneAbbreviations =
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
  , TimeZoneAbbreviationInfo "BRT"   (hours -03)       "Brasília Time"
  , TimeZoneAbbreviationInfo "BRST"  (hours -02)       "Brasília Summer Time"
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
