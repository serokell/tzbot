-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.TzBot.CheckForClockChangesSpec
  ( test_checkForClockChanges'
  , test_checkForClockChanges
  ) where

import TzPrelude

import Data.Time (UTCTime)
import Data.Time.TZInfo
import Data.Time.TZInfo qualified as TZI
import Data.Time.TZTime (toUTC)
import Data.Time.TZTime.QQ (tz)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Text.Interpolation.Nyan

import TzBot.Parser (parseTimeRefs)
import TzBot.TimeReference (TimeReferenceToUTCResult(..), timeReferenceToUTC)
import TzBot.TZ (ClockChange(..), checkForClockChanges, checkForClockChanges')
import TzBot.Util

springHavana2022utc, autumnHavana2022utc, springHavana2023utc :: UTCTime
-- https://www.timeanddate.com/time/change/cuba/havana?year=2022
springHavana2022utc = toUTC [tz|2022-03-13 05:00:00 [UTC]|]
autumnHavana2022utc = toUTC [tz|2022-11-06 05:00:00 [UTC]|]
-- https://www.timeanddate.com/time/change/cuba/havana?year=2023
springHavana2023utc = toUTC [tz|2023-03-12 05:00:00 [UTC]|]

springHavana2022, autumnHavana2022, springHavana2023 :: ClockChange
springHavana2022 = ClockChange springHavana2022utc cst cdt (TZI.fromLabel America__Havana)
autumnHavana2022 = ClockChange autumnHavana2022utc cdt cst (TZI.fromLabel America__Havana)
springHavana2023 = ClockChange springHavana2023utc cst cdt (TZI.fromLabel America__Havana)

cst, cdt :: Offset
cst = Offset ((-5)*60)
cdt = Offset ((-4)*60)

test_checkForClockChanges' :: TestTree
test_checkForClockChanges' = testGroup "checkForClockChanges'"
  [ testCase "havana1" $ do
      let t1 = toUTC [tz|2022-03-12 23:59:00 [America/Havana]|]
      let t2 = toUTC [tz|2023-03-12 01:01:00 [America/Havana]|]
      let ts = checkForClockChanges' (TZI.fromLabel America__Havana) t1 t2
      ts @?=
        [springHavana2022, autumnHavana2022, springHavana2023]
  , testCase "havana2" $ do
      let t1 = toUTC [tz|2022-03-13 01:00:00 [America/Havana]|]
      let t2 = toUTC [tz|2023-03-12 01:00:00 [America/Havana]|]
      let ts = checkForClockChanges' (TZI.fromLabel America__Havana) t1 t2
      ts @?=
        [autumnHavana2022, springHavana2023]
  , testCase "utc" $ do
      let t1 = toUTC [tz|1900-01-01 01:00:00 [UTC]|]
      let t2 = toUTC [tz|2024-03-12 01:00:00 [UTC]|]
      let ts = checkForClockChanges' (TZI.fromLabel Etc__UTC) t1 t2
      ts @?= []
  , testCase "incorrect arguments" $ do
      let t1 = toUTC [tz|1900-01-01 01:00:00 [UTC]|]
      let t2 = toUTC [tz|2024-03-12 01:00:00 [UTC]|]
      let ts = checkForClockChanges' (TZI.fromLabel Etc__UTC) t2 t1
      ts @?= []
  ]

test_checkForClockChanges :: TestTree
test_checkForClockChanges =
  testGroup "checkForClockChanges"
    [ testCase "When the date is not specified, check for offset changes up to 3 days after/before the inferred date" do
        -- there's an offset change 2 days and 23 hours before the inferred date/time.
        -- https://www.timeanddate.com/time/change/cuba/havana?year=2023
        check
          (toUTC [tz|2023-03-14 00:00:00 [America/Havana]|]) "23:00" America__Havana
          Europe__Lisbon
          [ ClockChange (toUTC [tz|2023-03-12 01:00:00 [America/Havana]|]) cst cdt (TZI.fromLabel America__Havana) ]

        -- there's an offset change 3 days and 1 hour before, so we don't return it.
        check
          (toUTC [tz|2023-03-15 00:00:00 [America/Havana]|]) "01:00" America__Havana
          Europe__Lisbon
          []

        -- there's an offset change 2 days and 23 hours after the inferred date/time.
        -- https://www.timeanddate.com/time/change/cuba/havana?year=2023
        check
          (toUTC [tz|2023-03-09 00:00:00 [America/Havana]|]) "00:00" America__Havana
          Europe__Lisbon
          [ ClockChange (toUTC [tz|2023-03-12 01:00:00 [America/Havana]|]) cst cdt (TZI.fromLabel America__Havana) ]

        -- there's an offset change 3 days and 1 hour after, so we don't return it.
        check
          (toUTC [tz|2023-03-08 00:00:00 [America/Havana]|]) "23:00" America__Havana
          Europe__Lisbon
          []
    , testCase "When the sender specifies a day of week, check for offset changes during the week before the inferred date" do
        -- We will infer "23:00 on Saturday" means 23:00 on 18 March.
        -- There's an offset change 6 days and 23 hours before the inferred date/time.
        check
          (toUTC [tz|2023-03-14 00:00:00 [America/Havana]|]) "23:00 on Saturday" America__Havana
          Europe__Lisbon
          [ ClockChange (toUTC [tz|2023-03-12 01:00:00 [America/Havana]|]) cst cdt (TZI.fromLabel America__Havana) ]

        -- We will infer "01:00 on Sunday" means 01:00 on 19 March.
        -- There's an offset change 7 days and 1 hour before the inferred date/time, so we don't return it.
        check
          (toUTC [tz|2023-03-14 00:00:00 [America/Havana]|]) "01:00 on Sunday" America__Havana
          Europe__Lisbon
          []

        -- We will infer "23:00 on Saturday" means 01:00 on 11 March.
        -- There's an offset change 1 hour after the inferred date/time,
        -- but we only check for offset changes in the past, so we don't return anything.
        check
          (toUTC [tz|2023-03-11 00:00:00 [America/Havana]|]) "23:00 on Saturday" America__Havana
          Europe__Lisbon
          []
    ]
  where
    check :: UTCTime -> Text -> TZLabel -> TZLabel -> [ClockChange] -> Assertion
    check now input senderTimeZone receiverTimeZone expectedClockChanges = do
      case parseTimeRefs input of
        [timeRef] ->
          case timeReferenceToUTC senderTimeZone now timeRef of
            TRTUSuccess timeRefSuccess ->
              checkForClockChanges timeRef timeRefSuccess receiverTimeZone @?= expectedClockChanges
            other ->
              assertFailure
                [int|s|
                  Expected 'timeReferenceToUTC' to success, but it failed:
                  #s{other}
                |]
        timeRefs ->
          assertFailure
            [int|s|
              Expected 'parseTimeRefs' to return exactly 1 result, but it returned:
              #s{timeRefs}
            |]
