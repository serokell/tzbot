{- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.TzBot.TimeReferenceToUtcSpec
  ( test_TimeReferenceToUtc
  ) where

import Universum

import Data.Time
import Data.Time.TZInfo (TZLabel(America__Havana, Asia__Tashkent, Europe__Helsinki))
import Data.Time.TZInfo qualified as TZI
import Data.Time.TZTime (TZTime, toUTC)
import Data.Time.TZTime.QQ (tz)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import TzBot.TimeReference
import TzBot.Util

time1 :: UTCTime
time1 = toUTC [tz|2022-12-14 10:30:00 [Europe/Helsinki]|] -- Wednesday

label1 :: TZLabel
label1 = Europe__Helsinki

time2 :: UTCTime
time2 = toUTC [tz|2022-03-11 10:30:00 [America/Havana]|]

time3 :: UTCTime
time3 = toUTC [tz|2022-11-04 10:30:00 [America/Havana]|]

arbitraryTime :: TZTime
arbitraryTime = [tz|2022-02-02 10:00:00 [UTC]|]

labelHavana :: TZLabel
labelHavana = America__Havana

arbitraryTZInfo :: TZI.TZInfo
arbitraryTZInfo = TZI.fromLabel labelHavana

hour :: Int
hour = 60

data TestEntry = TestEntry
  { teTimeRef :: TimeReference
  , teUserLabel :: TZLabel
  , teCurrentTime :: UTCTime
  , teResult :: TimeReferenceToUTCResult
  }

newtype TranslateWrapper = TranslateWrapper TimeReferenceToUTCResult
  deriving stock (Show)

comparePartlyTimeRefSuccess :: TimeRefSuccess -> TimeRefSuccess -> Bool
comparePartlyTimeRefSuccess t1 t2 =
  trsUtcResult t1 == trsUtcResult t2
  && trsOriginalDate t1 == trsOriginalDate t2

instance Eq TranslateWrapper where
  (TranslateWrapper v1) == (TranslateWrapper v2) = case (v1, v2) of
    (TRTUSuccess s1, TRTUSuccess s2) -> comparePartlyTimeRefSuccess s1 s2
    (TRTUInvalid g1, TRTUInvalid g2) -> ((==) `on` giErrorInfo) g1 g2
    (TRTUAmbiguous o1, TRTUAmbiguous o2) -> ((==) `on` oiErrorInfo) o1 o2
    (TRTUInvalidTimeZoneAbbrev a1, TRTUInvalidTimeZoneAbbrev a2) -> a1 == a2
    (_, _) -> False

mkTestCase :: TestEntry -> Assertion
mkTestCase TestEntry {..} = do
  let res = timeReferenceToUTC teUserLabel teCurrentTime teTimeRef
  TranslateWrapper res @?= TranslateWrapper teResult

utcToUtcLocalTime :: UTCTime -> LocalTime
utcToUtcLocalTime = utcToLocalTime utc

mkSuccessSameDate :: UTCTime -> TimeRefSuccess
mkSuccessSameDate refTime =
  TimeRefSuccess refTime
    arbitraryTZInfo
    (localDay $ utcToUtcLocalTime refTime)

test_TimeReferenceToUtc :: TestTree
test_TimeReferenceToUtc = testGroup "TimeReference to UTC" $
  [ testGroup "Time reference without date and location references"
    [ testCase "Today" $
        mkTestCase $ TestEntry
          { teTimeRef =
              TimeReference "" (TimeOfDay 20 30 0) Nothing Nothing
          , teUserLabel = label1
          , teCurrentTime = time1
          , teResult =
              TRTUSuccess $ mkSuccessSameDate
                (toUTC [tz|2022-12-14 18:30:00 [UTC]|])
          }
    , testCase "Tomorrow" $
        mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 8 0 0) Nothing Nothing
          , teUserLabel = label1
          , teCurrentTime = time1
          , teResult =
              TRTUSuccess $ mkSuccessSameDate
                (toUTC [tz|2022-12-15 06:00:00 [UTC]|])
          }
    ]
  , testGroup "Time reference without location reference" $
    [ testCase "Days from today" $
        mkTestCase $ TestEntry
          { teTimeRef =
              TimeReference "" (TimeOfDay 8 0 0) (Just $ DaysFromToday 1) Nothing
          , teUserLabel = label1
          , teCurrentTime = time1
          , teResult =
              TRTUSuccess $ mkSuccessSameDate
                (toUTC [tz|2022-12-15 06:00:00 [UTC]|])
          }
    , testCase "Day of week, Wednesday -> Monday" $
        mkTestCase $ TestEntry
          { teTimeRef =
              TimeReference "" (TimeOfDay 8 0 0) (Just $ DayOfWeekRef Monday) Nothing
          , teUserLabel = label1
          , teCurrentTime = time1
          , teResult =
              TRTUSuccess $ mkSuccessSameDate
                (toUTC [tz|2022-12-19 06:00:00 [UTC]|])
          }
    , testCase "Day of week, Wednesday -> Friday" $
        mkTestCase $ TestEntry
          { teTimeRef =
              TimeReference "" (TimeOfDay 8 0 0) (Just $ DayOfWeekRef Friday) Nothing
          , teUserLabel = label1
          , teCurrentTime = time1
          , teResult =
              TRTUSuccess $ mkSuccessSameDate
                (toUTC [tz|2022-12-16 06:00:00 [UTC]|])
          }
    , testCase "Day of week, Wednesday -> Wednesday" $
        mkTestCase $ TestEntry
          { teTimeRef =
              TimeReference "" (TimeOfDay 8 0 0) (Just $ DayOfWeekRef Wednesday) Nothing
          , teUserLabel = label1
          , teCurrentTime = time1
          , teResult =
              TRTUSuccess $ mkSuccessSameDate
                (toUTC [tz|2022-12-14 06:00:00 [UTC]|])
          }

    , testCase "Day of month, new year" $
        mkTestCase $ TestEntry
          { teTimeRef =
              TimeReference "" (TimeOfDay 8 0 0) (Just $ DayOfMonthRef 2 Nothing) Nothing
          , teUserLabel = label1
          , teCurrentTime = time1
          , teResult =
              TRTUSuccess $ mkSuccessSameDate
                (toUTC [tz|2023-01-02 06:00:00 [UTC]|])
          }
    , testCase "Day of month, search for exactly matching candidate (not the closest)" $
        mkTestCase $ TestEntry
          { teTimeRef =
              TimeReference "" (TimeOfDay 8 0 0) (Just $ DayOfMonthRef 31 Nothing) Nothing
          , teUserLabel = label1
          , teCurrentTime = toUTC [tz|2022-11-29 10:00:00 [Europe/Helsinki]|]
          , teResult =
              TRTUSuccess $ mkSuccessSameDate
                (toUTC [tz|2022-12-31 06:00:00 [UTC]|])
          }
    , testCase "Day of month, prefer past days if they are much closer" $
        mkTestCase $ TestEntry
          { teTimeRef =
              TimeReference "" (TimeOfDay 8 0 0) (Just $ DayOfMonthRef 25 Nothing) Nothing
          , teUserLabel = label1
          , teCurrentTime = toUTC [tz|2022-12-03 10:00:00 [Europe/Helsinki]|]
          , teResult =
              TRTUSuccess $ mkSuccessSameDate
                (toUTC [tz|2022-11-25 06:00:00 [UTC]|])
          }
    , testCase "Day of month, prefer future days if they are slightly further" $
        mkTestCase $ TestEntry
          { teTimeRef =
              TimeReference "" (TimeOfDay 8 0 0) (Just $ DayOfMonthRef 22 Nothing) Nothing
          , teUserLabel = label1
          , teCurrentTime = toUTC [tz|2022-12-03 10:00:00 [Europe/Helsinki]|]
          , teResult =
              TRTUSuccess $ mkSuccessSameDate
                (toUTC [tz|2022-12-22 06:00:00 [UTC]|])
          }

    , testCase "Day of month and month of year, 1" $
        mkTestCase $ TestEntry
          { teTimeRef =
              TimeReference "" (TimeOfDay 8 0 0) (Just $ DayOfMonthRef 14 $ Just (1, Nothing)) Nothing
          , teUserLabel = label1
          , teCurrentTime = time1
          , teResult =
              TRTUSuccess $ mkSuccessSameDate
                (toUTC [tz|2023-01-14 06:00:00 [UTC]|])
          }
    , testCase "Day of month and month of year, prefer future day if it's further" $
        mkTestCase $ TestEntry
          { teTimeRef =
              TimeReference "" (TimeOfDay 0 30 0) (Just $ DayOfMonthRef 10 $ Just (2, Nothing)) Nothing
          , teUserLabel = label1
          , teCurrentTime = toUTC [tz|2022-06-15 10:00:00 [Europe/Helsinki]|]
          , teResult =
              TRTUSuccess $ TimeRefSuccess
                (toUTC [tz|2023-02-09 22:30:00 [UTC]|])
                arbitraryTZInfo
                (fromGregorian 2023 02 10)
          }
    , testCase "Day of month and month of year, prefer past day if it's much closer" $
        mkTestCase $ TestEntry
          { teTimeRef =
              TimeReference "" (TimeOfDay 8 0 0) (Just $ DayOfMonthRef 10 $ Just (4, Nothing)) Nothing
          , teUserLabel = label1
          , teCurrentTime = toUTC [tz|2022-06-15 10:00:00 [Europe/Helsinki]|]
          , teResult =
              TRTUSuccess $ mkSuccessSameDate
                (toUTC [tz|2022-04-10 05:00:00 [UTC]|])
          }

    , testCase "Time reference without location reference,\
                            \ day of month and month of year, same day" $
        mkTestCase $ TestEntry
          { teTimeRef =
              TimeReference "" (TimeOfDay 8 0 0) (Just $ DayOfMonthRef 14 $ Just (12, Nothing)) Nothing
          , teUserLabel = label1
          , teCurrentTime = time1
          , teResult =
              TRTUSuccess $ mkSuccessSameDate
                (toUTC [tz|2022-12-14 06:00:00 [UTC]|])
          }
    ]
  , testCase "Custom time ref" $
      mkTestCase $ TestEntry
          { teTimeRef =
              TimeReference "" (TimeOfDay 8 0 0)
                (Just $ DaysFromToday 2) (Just $ TimeZoneRef Asia__Tashkent)
          , teUserLabel = label1
          , teCurrentTime = time1
          , teResult =
              TRTUSuccess $ mkSuccessSameDate
                (toUTC [tz|2022-12-16 03:00:00 [UTC]|])
          }
  , testCase "Explicit offset" $ do
      let offset = Offset $ 3 * hour
      mkTestCase $ TestEntry
          { teTimeRef =
              TimeReference "" (TimeOfDay 8 0 0)
                (Just $ DaysFromToday 2) (Just $ OffsetRef offset)
          , teUserLabel = label1
          , teCurrentTime = time1
          , teResult =
              TRTUSuccess $ mkSuccessSameDate
                (toUTC [tz|2022-12-16 05:00:00 [UTC]|])
          }
  , testCase "Valid timezone abbreviation" $
      mkTestCase $ TestEntry
        { teTimeRef = TimeReference "" (TimeOfDay 8 0 0) (Just $ DaysFromToday 2)
                      (Just $ OffsetRef (Offset $ 3 * 60))
        , teUserLabel = label1
        , teCurrentTime = time1
        , teResult =
          TRTUSuccess $ mkSuccessSameDate
            (toUTC [tz|2022-12-16 05:00:00 [UTC]|])
        }
  , testCase "Invalid timezone abbreviation" $
      mkTestCase $ TestEntry
        { teTimeRef =
            TimeReference "" (TimeOfDay 8 0 0) (Just $ DaysFromToday 2)
              (Just $ UnknownTimeZoneAbbreviationRef $ UnknownTimeZoneAbbrev "MKS" ["MSK"])
        , teUserLabel = label1
        , teCurrentTime = time1
        , teResult = TRTUInvalidTimeZoneAbbrev $ UnknownTimeZoneAbbrev "MKS" ["MSK"]
        }
  , testGroup "Timeshift subtleties" $
    [ testCase "Turn on DST, explicit time zone, Havana, Cuba" $
        mkTestCase $ TestEntry
          { teTimeRef =
              TimeReference "" (TimeOfDay 0 30 0)
                (Just $ DaysFromToday 2) (Just $ TimeZoneRef labelHavana)
          , teUserLabel = labelHavana
          , teCurrentTime = time2
          , teResult = TRTUInvalid $ GapInfo arbitraryTime arbitraryTime $
            TimeShiftErrorInfo (fromGregorian 2022 3 13)
          }
    , testCase "Turn on DST, explicit offset, Havana, Cuba" $ do
        let offset = Offset $ hour * (-5)
        mkTestCase $ TestEntry
          { teTimeRef =
              TimeReference "" (TimeOfDay 0 30 0)
                (Just $ DaysFromToday 2) (Just $ OffsetRef offset)
          , teUserLabel = labelHavana
          , teCurrentTime = time2
          , teResult = TRTUSuccess $ mkSuccessSameDate
              (toUTC [tz|2022-03-13 05:30:00 [UTC]|])
          }
    , testCase "Turn on DST, explicit offset abbreviation, Havana, Cuba" $ do
        mkTestCase $ TestEntry
          { teTimeRef =
              TimeReference "" (TimeOfDay 0 30 0) (Just $ DaysFromToday 2)
                (Just $ OffsetRef (Offset ((-5) * 60)))
          , teUserLabel = labelHavana
          , teCurrentTime = time2
          , teResult = TRTUSuccess $ mkSuccessSameDate
              (toUTC [tz|2022-03-13 05:30:00 [UTC]|])
          }
    , testCase "Turn off DST, explicit time zone, Havana, Cuba" $
        mkTestCase $ TestEntry
          { teTimeRef =
              TimeReference "" (TimeOfDay 0 30 0)
                (Just $ DaysFromToday 2) (Just $ TimeZoneRef labelHavana)
          , teUserLabel = labelHavana
          , teCurrentTime = time3
          , teResult = TRTUAmbiguous $ OverlapInfo arbitraryTime arbitraryTime $
            TimeShiftErrorInfo (fromGregorian 2022 11 6)
          }
    , testCase "Turn off DST, explicit offset, Havana, Cuba" $ do
        let offset = Offset $ hour * (-5)
        mkTestCase $ TestEntry
          { teTimeRef =
              TimeReference "" (TimeOfDay 0 30 0)
                (Just $ DaysFromToday 2) (Just $ OffsetRef offset)
          , teUserLabel = labelHavana
          , teCurrentTime = time3
          , teResult = TRTUSuccess $ mkSuccessSameDate
              (toUTC [tz|2022-11-06 05:30:00 [UTC]|])
          }
    , testCase "Turn off DST, explicit offset abbreviation, Havana, Cuba" $ do
        mkTestCase $ TestEntry
          { teTimeRef =
              TimeReference "" (TimeOfDay 0 30 0) (Just $ DaysFromToday 2)
                (Just $ OffsetRef (Offset ((-5) * 60)))
          , teUserLabel = labelHavana
          , teCurrentTime = time3
          , teResult = TRTUSuccess $ mkSuccessSameDate
              (toUTC [tz|2022-11-06 05:30:00 [UTC]|])
          }
    ]
  ]
