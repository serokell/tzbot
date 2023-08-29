-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.TZ
  ( ClockChange (..)
  , checkForClockChanges
  , checkForClockChanges'
  , findLastClockChange
  ) where

import TzPrelude

import Data.Bits (shiftR)
import Data.List (nub)
import Data.List qualified
import Data.Time (TimeZone(TimeZone, timeZoneMinutes), UTCTime, addUTCTime, nominalDay)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.TZInfo as TZI
import Data.Time.TZTime (TZTime, toUTC, tzTimeTZInfo)
import Data.Time.Zones (TZ)
import Data.Time.Zones.Internal (utcTimeToInt64)
import Data.Time.Zones.Types (TZ(..))
import Data.Vector qualified as VB
import Data.Vector.Unboxed qualified as VU

import TzBot.TimeReference (DateReference(..), TimeRefSuccess(..), TimeReference(..))
import TzBot.Util (NamedOffset, Offset(..))

-- | Represents a specific change in offset.
data ClockChange = ClockChange
  { ccUTCTime :: UTCTime
  -- ^ The time at which the offset change occurred / will occur.
  , ccBefore   :: Offset
  -- ^ The offset before the change.
  , ccAfter    :: Offset
  -- ^ The offset after the change.
  , ccTzInfo   :: TZI.TZInfo
  -- ^ The rules for the time zone where this change occurs.
  } deriving stock (Eq, Show)

-- | See the "Clock change warnings" section in `docs/development.md`.
-- Sometimes the user does not specify a date, and we have to infer which date he meant.
--
-- When we do this, we should check if there were any clock changes around the inferred date,
-- in either the "source" time zone or the receiver's time zone,
-- and, if so, emit a warning.
checkForClockChanges :: TimeReference -> TimeRefSuccess -> TZLabel -> [ClockChange]
checkForClockChanges TimeReference{trDateRef} TimeRefSuccess{trsUtcResult, trsTzInfo} receiverTzLabel =
  case trDateRef of
    -- The date was inferred: we should check if there were any clock changes 3 days before/after the inferred date.
    Nothing ->
      checkEachTimeZone
        (trsUtcResult & addUTCTime (nominalDay * -3))
        (trsUtcResult & addUTCTime (nominalDay * 3))
    -- The user specified the day of week; we had to infer which week they meant.
    --
    -- It's possible they meant "last week" instead, so we should check if
    -- there were any clock changes between last week and the date we inferred.
    Just (DayOfWeekRef {}) ->
      checkEachTimeZone
        (trsUtcResult & addUTCTime (nominalDay * -7))
        trsUtcResult
    -- The date was specified by the user; no need to perform the "clock change check"
    Just (DaysFromToday {}) -> []
    Just (DayOfMonthRef {}) -> []
  where
    -- Check for clock changes in both the "source" time zone and the receiver's time zone.
    checkEachTimeZone :: UTCTime -> UTCTime -> [ClockChange]
    checkEachTimeZone lowerBound upperBound =
      nub [trsTzInfo, TZI.fromLabel receiverTzLabel]
        & concatMap \tz -> checkForClockChanges' tz lowerBound upperBound

-- | Find last clock change for given `TZTime`.
-- Note that it should exist, otherwise the function will
-- throw an error. Use this function on times
-- contained inside `TZError`, next to the clock change:
-- * second occurrence of amgiguous time inside `TZOverlap` constructor;
-- * invalid time adjusted forward by the length of the gap for `TZGap` constructor.
findLastClockChange :: TZTime -> ClockChange
findLastClockChange tzt = do
  let ccTzInfo = tzTimeTZInfo tzt
      after = utcTimeToInt64 $ toUTC tzt
  let tz@(TZ trans _ _) = tziRules ccTzInfo
      ixb = binarySearch trans after
  if ixb == 0
  then error "no clock change found"
  else do
    let ccUTCTime = int64ToUtcTime $ trans VU.! ixb
        ccBefore = Offset $ timeZoneMinutes $ timeZoneForIx tz (ixb - 1)
        ccAfter = Offset $ timeZoneMinutes $ timeZoneForIx tz ixb
    ClockChange {..}

checkForClockChanges' :: TZInfo -> UTCTime -> UTCTime -> [ClockChange]
checkForClockChanges' tzInfo (utcTimeToInt64 -> before) (utcTimeToInt64 -> after)
  | before > after = []
  | otherwise = do
  let tz@(TZ trans _ _) = tziRules tzInfo
      ixb = binarySearch trans before
      startingFromIxb = VU.unsafeDrop ixb trans
      desiredClockChanges = zip [ixb..] $
        takeWhile (<= after) $ VU.toList startingFromIxb
      timeZones =
        map (\(ix, int64utc) ->
          ( int64ToUtcTime int64utc
          , Offset $ timeZoneMinutes $ timeZoneForIx tz ix)
          ) desiredClockChanges
      clockChanges = zip timeZones $ Data.List.tail timeZones
  map (\((_ub, b), (ua, a)) -> ClockChange ua b a tzInfo) clockChanges

int64ToUtcTime :: Int64 -> UTCTime
int64ToUtcTime = posixSecondsToUTCTime . fromIntegral
{-# INLINE int64ToUtcTime #-}

-- | Copied from "Data.Time.Zones" module of `tz` package because not exported.
timeZoneForIx :: TZ -> Int -> NamedOffset
timeZoneForIx (TZ _ diffs infos) i = TimeZone diffMins isDst name
  where
    diffMins = VU.unsafeIndex diffs i `div` 60
    (isDst, name) = VB.unsafeIndex infos i
{-# INLINE timeZoneForIx #-}

-- | Copied from "Data.Time.Zones" module of `tz` package because not exported.
-- Returns the largest index `i` such that `v ! i <= t`.
--
-- Assumes that `v` is sorted, has at least one element and `v ! 0 <= t`.
binarySearch :: (VU.Unbox a, Ord a) => VU.Vector a -> a -> Int
binarySearch v t | n == 1    = 0
                 | otherwise = t `seq` go 1 n
  where
    n = VU.length v
    go !l !u | l >= u = l - 1
             | VU.unsafeIndex v k <= t  = go (k + 1) u
             | otherwise  = go l k
      where
        k = (l + u) `shiftR` 1
{-# INLINE binarySearch #-}
