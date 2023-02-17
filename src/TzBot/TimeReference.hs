-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.TimeReference where

import Universum

import Control.Arrow ((>>>))
import Data.List.NonEmpty qualified as NE
import Data.Time
import Data.Time.Calendar.Compat
import Data.Time.TZInfo qualified as TZI
import Data.Time.TZTime qualified as TZT
import Data.Time.Zones.All (TZLabel)
import Formatting (Buildable)

import TzBot.Util (Offset(..), tzInfoFromOffset)

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
type TimeReferenceText = Text

-- | Datatype for keeping value together with its parsed text (as a sequence of tokens)
data Matched a = Matched
  { mtText :: Text
    -- ^ Consumed text
  , mtValue :: a
    -- ^ Parsed value
  } deriving stock (Show, Eq, Generic, Functor, Foldable, Traversable)

-- TODO: use lenses
modifyText :: (Text -> Text) -> Matched a -> Matched a
modifyText f Matched {..} = Matched {mtText = f mtText, ..}

type family WhetherMatched f x where
  WhetherMatched Identity x = x
  WhetherMatched Matched  x = Matched x

-- | A reference to a point in time, e.g. "tuesday at 10am", "3pm CST on July 7th"
data TimeReferenceGeneric f = TimeReference
  { trText :: TimeReferenceText -- ^ The original section of the text from where this `TimeReference` was parsed.
  , trTimeOfDay :: TimeOfDay
  , trDateRef :: Maybe (WhetherMatched f DateReference)
  , trLocationRef :: Maybe (WhetherMatched f LocationReference)
  }

deriving stock instance Show TimeReference
deriving stock instance Eq TimeReference
deriving stock instance Show TimeReferenceMatched
deriving stock instance Eq TimeReferenceMatched

type TimeReference = TimeReferenceGeneric Identity
type TimeReferenceMatched = TimeReferenceGeneric Matched

matchedToPlain :: TimeReferenceMatched -> TimeReference
matchedToPlain TimeReference {..} = TimeReference
  { trDateRef = fmap mtValue trDateRef
  , trLocationRef = fmap mtValue trLocationRef
  , ..
  }

getTzLabelMaybe :: TZLabel -> TimeReference -> Maybe TZLabel
getTzLabelMaybe senderTz timeRef = case trLocationRef timeRef of
  Just (TimeZoneRef refTzLabel) -> Just refTzLabel
  Nothing -> Just senderTz
  _ -> Nothing

data DateReference
  = DaysFromToday Int
  | DayOfWeekRef DayOfWeek
  | DayOfMonthRef DayOfMonth (Maybe (MonthOfYear, Maybe Year))
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
  case eithTzInfo of
    Left abbrev -> TRTUInvalidTimeZoneAbbrev abbrev
    Right tzInfo -> do
      let eventLocalTime = TZT.fromUTC tzInfo eventTimestamp
      let eithRefTime = eventLocalTime & TZT.modifyLocalStrict (
            dayTransition >>> TZT.atTimeOfDay trTimeOfDay
            )
      case eithRefTime of
        Left err -> tzErrorToResult err
        Right refTime -> TRTUSuccess $ TimeRefSuccess
            { trsUtcResult = TZT.toUTC refTime
            , trsTzInfo = tzInfo
            , trsOriginalDate = localDay $ TZT.tzTimeLocalTime refTime
            }
  where
  tzErrorToResult :: TZT.TZError -> TimeReferenceToUTCResult
  tzErrorToResult = \case
    TZT.TZOverlap invalidTime first_ second_ -> TRTUAmbiguous $
      OverlapInfo first_ second_ $
        TimeShiftErrorInfo (localDay invalidTime)
    TZT.TZGap invalidTime first_ second_ -> TRTUInvalid $
      GapInfo first_ second_ $
        TimeShiftErrorInfo (localDay invalidTime)
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
      Just (monthOfYear, mbYear) -> case mbYear of
        Nothing -> chooseBestYear dayOfMonth monthOfYear eventLocalTime
        Just year ->
          TZT.atDay (fromGregorian year monthOfYear dayOfMonth) eventLocalTime

  eithTzInfo :: Either UnknownTimeZoneAbbrev TZI.TZInfo
  eithTzInfo = case trLocationRef of
    Nothing -> pure $ TZI.fromLabel sendersTZLabel
    Just (TimeZoneRef tzLabel) -> pure $ TZI.fromLabel tzLabel
    Just (OffsetRef offset) -> pure $ tzInfoFromOffset offset
    Just (TimeZoneAbbreviationRef abbrev) -> pure $ tzInfoFromOffset $ tzaiOffsetMinutes abbrev
    Just (UnknownTimeZoneAbbreviationRef unknownAbbrev) -> Left unknownAbbrev

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
  { trsUtcResult :: UTCTime
    -- ^ The result of the conversion.
  , trsTzInfo :: TZI.TZInfo
    -- ^ TZInfo, obtained either from a TZLabel or from a static offset.
  , trsOriginalDate :: Day
    -- ^ The date _before_ the conversion to UTC.
    -- This date is in the timezone/offset specified by the user, if one was specified,
    -- or in the sender's current timezone otherwise.
  } deriving stock (Eq, Show)

newtype TimeShiftErrorInfo = TimeShiftErrorInfo
  { tseiOriginalDate :: Day
    -- ^ The day that was originally mentioned by the sender
    -- in specified or implicit sender's timezone.
  } deriving stock (Eq, Show)

-- | Result of converting `LocalTime` that can be possible
-- with two different offsets.
data OverlapInfo = OverlapInfo
  { oiFirstOccurrence :: TZT.TZTime
    -- ^ Instance with the earlier offset.
  , oiSecondOccurrence :: TZT.TZTime
    -- ^ Instance with the later offset.
  , oiErrorInfo :: TimeShiftErrorInfo
  } deriving stock (Eq, Show)

data GapInfo = GapInfo
  { giPreviousTime :: TZT.TZTime
  -- ^ The local time specified by the user shifted backward by the length of the gap.
  -- E.g., if they said "2:30am" but the clocks were turned forward 1 hour at 2am (such that 2:30am did not occur),
  -- then this field would be 2:30am - 1 hour = 1:30am.
  , giNextTime :: TZT.TZTime
  -- ^ The local time specified by the user shifted forward by the length of the gap.
  -- E.g., if they said "2:30am" but the clocks were turned forward 1 hour at 2am (such that 2:30am did not occur),
  -- then this field would be 2:30am + 1 hour = 3:30am.
  , giErrorInfo :: TimeShiftErrorInfo
  } deriving stock (Show, Generic, Eq)


data TimeReferenceToUTCResult
  = TRTUSuccess TimeRefSuccess
    -- ^ Conversion succeeded.
  | TRTUAmbiguous OverlapInfo
  -- ^ The time reference was ambiguous (e.g. due to a time ocurring twice in the same timezone during DST changes).
  -- See [Edge cases & pitfalls](https://github.com/serokell/tzbot/blob/main/docs/pitfalls.md#ambiguous-times).
  | TRTUInvalid GapInfo
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
  , tzaiOffsetMinutes :: Offset
    -- ^ Offset from UTC in minutes.
  , tzaiFullName :: Text
  } deriving stock (Eq, Show)
