
{- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.TzBot.TimeReferenceToUtcSpec (test_TimeReferenceToUtc) where

import Universum

import Data.Time (DayOfWeek(Friday, Monday, Wednesday), TimeOfDay(TimeOfDay), UTCTime)
import Data.Time.TZInfo (TZLabel(America__Havana, Asia__Tashkent, Europe__Helsinki))
import Data.Time.TZTime (toUTC)
import Data.Time.TZTime.QQ (tz)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import TzBot.TimeReference

time1 :: UTCTime
time1 = toUTC [tz|2022-12-14 10:30:00 [Europe/Helsinki]|] -- Wednesday

label1 :: TZLabel
label1 = Europe__Helsinki

time2 :: UTCTime
time2 = toUTC [tz|2022-03-11 10:30:00 [America/Havana]|]

time3 :: UTCTime
time3 = toUTC [tz|2022-11-04 10:30:00 [America/Havana]|]

labelHavana :: TZLabel
labelHavana = America__Havana

hour :: Int
hour = 60

data TestEntry = TestEntry
  { teTimeRef :: TimeReference
  , teUserLabel :: TZLabel
  , teCurrentTime :: UTCTime
  , teResult :: TimeReferenceToUTCResult
  }

mkTestCase :: TestEntry -> Assertion
mkTestCase TestEntry {..} = do
  let res = timeReferenceToUTC teUserLabel teCurrentTime teTimeRef
  res @?= teResult

test_TimeReferenceToUtc :: TestTree
test_TimeReferenceToUtc = testGroup "TimeReference to UTC" $
  [ testGroup "Time reference without date and location references"
    [ testCase "Today" $
        mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 20 30 0) Nothing Nothing
          , teUserLabel = label1
          , teCurrentTime = time1
          , teResult = TRTUSuccess (toUTC [tz|2022-12-14 18:30:00 [UTC]|]) (Left label1)
          }
    , testCase "Tomorrow" $
        mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 8 0 0) Nothing Nothing
          , teUserLabel = label1
          , teCurrentTime = time1
          , teResult = TRTUSuccess (toUTC [tz|2022-12-15 06:00:00 [UTC]|]) (Left label1)
          }
    ]
  , testGroup "Time reference without location reference" $
    [ testCase "Days from today" $
        mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 8 0 0) (Just $ DaysFromToday 1) Nothing
          , teUserLabel = label1
          , teCurrentTime = time1
          , teResult = TRTUSuccess (toUTC [tz|2022-12-15 06:00:00 [UTC]|]) (Left label1)
          }
    , testCase "Day of week, Wednesday -> Monday" $
        mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 8 0 0) (Just $ DayOfWeekRef Monday) Nothing
          , teUserLabel = label1
          , teCurrentTime = time1
          , teResult = TRTUSuccess (toUTC [tz|2022-12-19 06:00:00 [UTC]|]) (Left label1)
          }
    , testCase "Day of week, Wednesday -> Friday" $
        mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 8 0 0) (Just $ DayOfWeekRef Friday) Nothing
          , teUserLabel = label1
          , teCurrentTime = time1
          , teResult = TRTUSuccess (toUTC [tz|2022-12-16 06:00:00 [UTC]|]) (Left label1)
          }
    , testCase "Day of week, Wednesday -> Wednesday" $
        mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 8 0 0) (Just $ DayOfWeekRef Wednesday) Nothing
          , teUserLabel = label1
          , teCurrentTime = time1
          , teResult = TRTUSuccess (toUTC [tz|2022-12-14 06:00:00 [UTC]|]) (Left label1)
          }

    , testCase "Day of month, new year" $
        mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 8 0 0) (Just $ DayOfMonthRef 2 Nothing) Nothing
          , teUserLabel = label1
          , teCurrentTime = time1
          , teResult = TRTUSuccess (toUTC [tz|2023-01-02 06:00:00 [UTC]|]) (Left label1)
          }
    , testCase "Day of month, search for exactly matching candidate (not the closest)" $
        mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 8 0 0) (Just $ DayOfMonthRef 31 Nothing) Nothing
          , teUserLabel = label1
          , teCurrentTime = toUTC [tz|2022-11-29 10:00:00 [Europe/Helsinki]|]
          , teResult = TRTUSuccess (toUTC [tz|2022-12-31 06:00:00 [UTC]|]) (Left label1)
          }
    , testCase "Day of month, prefer past days if they are much closer" $
        mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 8 0 0) (Just $ DayOfMonthRef 25 Nothing) Nothing
          , teUserLabel = label1
          , teCurrentTime = toUTC [tz|2022-12-03 10:00:00 [Europe/Helsinki]|]
          , teResult = TRTUSuccess (toUTC [tz|2022-11-25 06:00:00 [UTC]|]) (Left label1)
          }
    , testCase "Day of month, prefer future days if they are slightly further" $
        mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 8 0 0) (Just $ DayOfMonthRef 22 Nothing) Nothing
          , teUserLabel = label1
          , teCurrentTime = toUTC [tz|2022-12-03 10:00:00 [Europe/Helsinki]|]
          , teResult = TRTUSuccess (toUTC [tz|2022-12-22 06:00:00 [UTC]|]) (Left label1)
          }

    , testCase "Day of month and month of year, 1" $
        mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 8 0 0) (Just $ DayOfMonthRef 14 (Just 1)) Nothing
          , teUserLabel = label1
          , teCurrentTime = time1
          , teResult = TRTUSuccess (toUTC [tz|2023-01-14 06:00:00 [UTC]|]) (Left label1)
          }
    , testCase "Day of month and month of year, prefer future day if it's further" $
        mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 8 0 0) (Just $ DayOfMonthRef 10 (Just 2)) Nothing
          , teUserLabel = label1
          , teCurrentTime = toUTC [tz|2022-06-15 10:00:00 [Europe/Helsinki]|]
          , teResult = TRTUSuccess (toUTC [tz|2023-02-10 06:00:00 [UTC]|]) (Left label1)
          }
    , testCase "Day of month and month of year, prefer past day if it's much closer" $
        mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 8 0 0) (Just $ DayOfMonthRef 10 (Just 4)) Nothing
          , teUserLabel = label1
          , teCurrentTime = toUTC [tz|2022-06-15 10:00:00 [Europe/Helsinki]|]
          , teResult = TRTUSuccess (toUTC [tz|2022-04-10 05:00:00 [UTC]|]) (Left label1)
          }

    , testCase "Time reference without location reference, day of month and month of year, same day" $
        mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 8 0 0) (Just $ DayOfMonthRef 14 (Just 12)) Nothing
          , teUserLabel = label1
          , teCurrentTime = time1
          , teResult = TRTUSuccess (toUTC [tz|2022-12-14 06:00:00 [UTC]|]) (Left label1)
          }
    ]
  , testCase "Custom time ref" $
      mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 8 0 0) (Just $ DaysFromToday 2) (Just $ TimeZoneRef Asia__Tashkent)
          , teUserLabel = label1
          , teCurrentTime = time1
          , teResult = TRTUSuccess (toUTC [tz|2022-12-16 03:00:00 [UTC]|]) (Left Asia__Tashkent)
          }
  , testCase "Explicit offset" $ do
      let offset = Offset $ 3 * hour
      mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 8 0 0) (Just $ DaysFromToday 2) (Just $ OffsetRef offset)
          , teUserLabel = label1
          , teCurrentTime = time1
          , teResult = TRTUSuccess (toUTC [tz|2022-12-16 05:00:00 [UTC]|]) (Right offset)
          }
  , testCase "Valid timezone abbreviation" $
      mkTestCase $ TestEntry
        { teTimeRef = TimeReference "" (TimeOfDay 8 0 0) (Just $ DaysFromToday 2) (Just $ TimeZoneAbbreviationRef $ TimeZoneAbbreviation "MSK")
        , teUserLabel = label1
        , teCurrentTime = time1
        , teResult = TRTUSuccess (toUTC [tz|2022-12-16 05:00:00 [UTC]|]) (Right $ Offset $ 3 * hour)
        }
  , testCase "Invalid timezone abbreviation" $
      mkTestCase $ TestEntry
        { teTimeRef = TimeReference "" (TimeOfDay 8 0 0) (Just $ DaysFromToday 2) (Just $ TimeZoneAbbreviationRef $ TimeZoneAbbreviation "MKS")
        , teUserLabel = label1
        , teCurrentTime = time1
        , teResult = TRTUInvalidTimeZoneAbbrev "MKS"
        }
  , testGroup "Timeshift subtleties" $
    [ testCase "Turn on DST, explicit time zone, Havana, Cuba" $
        mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 0 30 0) (Just $ DaysFromToday 2) (Just $ TimeZoneRef labelHavana)
          , teUserLabel = labelHavana
          , teCurrentTime = time2
          , teResult = TRTUInvalid
          }
    , testCase "Turn on DST, explicit offset, Havana, Cuba" $ do
        let offset = Offset $ hour * (-5)
        mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 0 30 0) (Just $ DaysFromToday 2) (Just $ OffsetRef offset)
          , teUserLabel = labelHavana
          , teCurrentTime = time2
          , teResult = TRTUSuccess (toUTC [tz|2022-03-13 05:30:00 [UTC]|]) (Right offset)
          }
    , testCase "Turn on DST, explicit offset abbreviation, Havana, Cuba" $ do
        let offset = Offset $ hour * (-5)
        mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 0 30 0) (Just $ DaysFromToday 2) (Just $ TimeZoneAbbreviationRef $ TimeZoneAbbreviation "CDT")
          , teUserLabel = labelHavana
          , teCurrentTime = time2
          , teResult = TRTUSuccess (toUTC [tz|2022-03-13 05:30:00 [UTC]|]) (Right offset)
          }
    , testCase "Turn off DST, explicit time zone, Havana, Cuba" $
        mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 0 30 0) (Just $ DaysFromToday 2) (Just $ TimeZoneRef labelHavana)
          , teUserLabel = labelHavana
          , teCurrentTime = time3
          , teResult = TRTUAmbiguous
          }
    , testCase "Turn off DST, explicit offset, Havana, Cuba" $ do
        let offset = Offset $ hour * (-5)
        mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 0 30 0) (Just $ DaysFromToday 2) (Just $ OffsetRef offset)
          , teUserLabel = labelHavana
          , teCurrentTime = time3
          , teResult = TRTUSuccess (toUTC [tz|2022-11-06 05:30:00 [UTC]|]) (Right offset)
          }
    , testCase "Turn off DST, explicit offset abbreviation, Havana, Cuba" $ do
        let offset = Offset $ hour * (-5)
        mkTestCase $ TestEntry
          { teTimeRef = TimeReference "" (TimeOfDay 0 30 0) (Just $ DaysFromToday 2) (Just $ TimeZoneAbbreviationRef $ TimeZoneAbbreviation "CDT")
          , teUserLabel = labelHavana
          , teCurrentTime = time3
          , teResult = TRTUSuccess (toUTC [tz|2022-11-06 05:30:00 [UTC]|]) (Right offset)
          }
    ]
  ]
