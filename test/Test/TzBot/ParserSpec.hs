-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.TzBot.ParserSpec
  ( test_parserSpec
  , test_Two_Time_References
  , test_Regression_Tests
  ) where

import TzPrelude

import Data.Text qualified as T
import Data.Time (DayOfWeek(..), TimeOfDay(..), defaultTimeLocale, formatTime)
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Test.Tasty.Runners (TestTree(TestGroup))
import Text.Interpolation.Nyan (int, rmode', rmode's)

import TzBot.Parser (parseTimeRefs)
import TzBot.TimeReference
  (DateReference(..), LocationReference(..), TimeReference(..), TimeZoneAbbreviationInfo(..))

{- | This test suite contains only tests whose output is too big
 - to be placed in doctests
 -}
test_parserSpec :: TestTree
test_parserSpec = TestGroup "ParserBig"
  [ testCase "First slashed term contains all references, second slashed term contains no refs" $
    mkTestCase
      "How about Wednesday at 10:00 / 11:00 UTC or 14:00 / 15:00"
      [ TimeReference
          "Wednesday at 10:00 UTC"
          (TimeOfDay 10 00 00)
          (Just (DayOfWeekRef Wednesday))
          (Just (TimeZoneAbbreviationRef (TimeZoneAbbreviationInfo {tzaiAbbreviation = "UTC", tzaiOffsetMinutes = 0, tzaiFullName = "UTC"})))
      , TimeReference
          "Wednesday 11:00 UTC"
          (TimeOfDay 11 00 00)
          (Just (DayOfWeekRef Wednesday))
          (Just (TimeZoneAbbreviationRef (TimeZoneAbbreviationInfo {tzaiAbbreviation = "UTC", tzaiOffsetMinutes = 0, tzaiFullName = "UTC"})))
      , TimeReference
          "14:00 (Wednesday) (UTC)"
          (TimeOfDay 14 00 00)
          (Just (DayOfWeekRef Wednesday))
          (Just (TimeZoneAbbreviationRef (TimeZoneAbbreviationInfo {tzaiAbbreviation = "UTC", tzaiOffsetMinutes = 0, tzaiFullName = "UTC"})))
      , TimeReference
          "15:00 (Wednesday) (UTC)"
          (TimeOfDay 15 00 00)
          (Just (DayOfWeekRef Wednesday))
          (Just (TimeZoneAbbreviationRef (TimeZoneAbbreviationInfo {tzaiAbbreviation = "UTC", tzaiOffsetMinutes = 0, tzaiFullName = "UTC"})))
      ]

  , testCase "First slashed term has day of week, second has also UTC" $
    mkTestCase
      "How about Wednesday at 10:00 / 11:00 OR 14:00 / 15:00 at Thursday UTC"
      [ TimeReference
          "Wednesday at 10:00 (UTC)"
          (TimeOfDay 10 00 00)
          (Just (DayOfWeekRef Wednesday))
          (Just (TimeZoneAbbreviationRef (TimeZoneAbbreviationInfo {tzaiAbbreviation = "UTC", tzaiOffsetMinutes = 0, tzaiFullName = "UTC"})))
      , TimeReference
          "Wednesday 11:00 (UTC)"
          (TimeOfDay 11 00 00)
          (Just (DayOfWeekRef Wednesday))
          (Just (TimeZoneAbbreviationRef (TimeZoneAbbreviationInfo {tzaiAbbreviation = "UTC", tzaiOffsetMinutes = 0, tzaiFullName = "UTC"})))
      , TimeReference
          "14:00 at Thursday UTC"
          (TimeOfDay 14 00 00)
          (Just (DayOfWeekRef Thursday))
          (Just (TimeZoneAbbreviationRef (TimeZoneAbbreviationInfo {tzaiAbbreviation = "UTC", tzaiOffsetMinutes = 0, tzaiFullName = "UTC"})))
      , TimeReference
          "15:00 at Thursday UTC"
          (TimeOfDay 15 00 00)
          (Just (DayOfWeekRef Thursday))
          (Just (TimeZoneAbbreviationRef (TimeZoneAbbreviationInfo {tzaiAbbreviation = "UTC", tzaiOffsetMinutes = 0, tzaiFullName = "UTC"})))
      ]
  , testCase "Some hyphenated intervals" $
    mkTestCase
      "Hi guys! Let’s have a sync call tomorrow? Almost every time from 7am-2pm UTC works (except 10:30am - 12pm UTC)"
      [ TimeReference
          "7am UTC"
          (TimeOfDay 07 00 00)
          (Nothing)
          (Just (TimeZoneAbbreviationRef (TimeZoneAbbreviationInfo {tzaiAbbreviation = "UTC", tzaiOffsetMinutes = 0, tzaiFullName = "UTC"})))
      , TimeReference
          "2pm UTC"
          (TimeOfDay 14 00 00)
          (Nothing)
          (Just (TimeZoneAbbreviationRef (TimeZoneAbbreviationInfo {tzaiAbbreviation = "UTC", tzaiOffsetMinutes = 0, tzaiFullName = "UTC"})))
      , TimeReference
          "10:30am UTC"
          (TimeOfDay 10 30 00)
          (Nothing)
          (Just (TimeZoneAbbreviationRef (TimeZoneAbbreviationInfo {tzaiAbbreviation = "UTC", tzaiOffsetMinutes = 0, tzaiFullName = "UTC"})))
      , TimeReference
          "12pm UTC"
          (TimeOfDay 12 00 00)
          (Nothing)
          (Just (TimeZoneAbbreviationRef (TimeZoneAbbreviationInfo {tzaiAbbreviation = "UTC", tzaiOffsetMinutes = 0, tzaiFullName = "UTC"})))
      ]
  ]

test_Two_Time_References :: [TestTree]
test_Two_Time_References =
  [ testCase "Each time reference can have its own date/location references" do
      mkTestCase
        "10am today in GMT and 11pm tomorrow in BST"
        [ TimeReference
            { trText = "10am today in GMT"
            , trTimeOfDay = TimeOfDay 10 00 00
            , trDateRef = Just
                ( DaysFromToday 0 )
            , trLocationRef = Just
                ( TimeZoneAbbreviationRef
                    ( TimeZoneAbbreviationInfo
                        { tzaiAbbreviation = "GMT"
                        , tzaiOffsetMinutes = 0
                        , tzaiFullName = "GMT"
                        }
                    )
                )
            }
        , TimeReference
            { trText = "11pm tomorrow in BST"
            , trTimeOfDay = TimeOfDay 23 00 00
            , trDateRef = Just
                ( DaysFromToday 1 )
            , trLocationRef = Just
                ( TimeZoneAbbreviationRef
                    ( TimeZoneAbbreviationInfo
                        { tzaiAbbreviation = "BST"
                        , tzaiOffsetMinutes = 60
                        , tzaiFullName = "British Summer Time"
                        }
                    )
                )
            }
        ]

  , testCase "Date/location references are shared" do
      mkTestCase
        "10am and 11pm today in BST"
        [ TimeReference
            { trText = "10am today in BST"
            , trTimeOfDay = TimeOfDay 10 00 00
            , trDateRef = Just
                ( DaysFromToday 0 )
            , trLocationRef = Just
                ( TimeZoneAbbreviationRef
                    ( TimeZoneAbbreviationInfo
                        { tzaiAbbreviation = "BST"
                        , tzaiOffsetMinutes = 60
                        , tzaiFullName = "British Summer Time"
                        }
                    )
                )
            }
        , TimeReference
            { trText = "11pm today in BST"
            , trTimeOfDay = TimeOfDay 23 00 00
            , trDateRef = Just
                ( DaysFromToday 0 )
            , trLocationRef = Just
                ( TimeZoneAbbreviationRef
                    ( TimeZoneAbbreviationInfo
                        { tzaiAbbreviation = "BST"
                        , tzaiOffsetMinutes = 60
                        , tzaiFullName = "British Summer Time"
                        }
                    )
                )
            }
        ]

  , testCase "Can have commas" do
      mkTestCase
        "10am and 11pm, today, in BST"
        [ TimeReference
            { trText = "10am, today, in BST"
            , trTimeOfDay = TimeOfDay 10 00 00
            , trDateRef = Just
                ( DaysFromToday 0 )
            , trLocationRef = Just
                ( TimeZoneAbbreviationRef
                    ( TimeZoneAbbreviationInfo
                        { tzaiAbbreviation = "BST"
                        , tzaiOffsetMinutes = 60
                        , tzaiFullName = "British Summer Time"
                        }
                    )
                )
            }
        , TimeReference
            { trText = "11pm, today, in BST"
            , trTimeOfDay = TimeOfDay 23 00 00
            , trDateRef = Just
                ( DaysFromToday 0 )
            , trLocationRef = Just
                ( TimeZoneAbbreviationRef
                    ( TimeZoneAbbreviationInfo
                        { tzaiAbbreviation = "BST"
                        , tzaiOffsetMinutes = 60
                        , tzaiFullName = "British Summer Time"
                        }
                    )
                )
            }
        ]
  ]

test_Regression_Tests :: [TestTree]
test_Regression_Tests =
  [ testCase "#89" do
      parseTimeRefs "7hff" @?= []
  ]

mkTestCase :: HasCallStack => Text -> [TimeReference] -> Assertion
mkTestCase input expectedRefs = do
  let outputRefs = parseTimeRefs input
  assertRefPairs expectedRefs outputRefs

assertRefPairs :: HasCallStack => [TimeReference] -> [TimeReference] -> Assertion
assertRefPairs [] [] = pass
assertRefPairs [] os =
  assertFailure
    [int||
      Expected no more time references, but got
      #{prettyPrintList os}
      |]
assertRefPairs es [] =
  assertFailure
    [int||
      Expected more time references #{prettyPrintList es}
      |]
assertRefPairs (e : es) (o : os) =
  assertRefPair e o >> assertRefPairs es os

prettyPrintTimeReference :: Char -> TimeReference -> Text
prettyPrintTimeReference start TimeReference {..} =
  [int|s|
  #{start} TimeReference
      #s{trText}
      (#{renderTimeOfDay trTimeOfDay})
      (#s{trDateRef})
      (#s{trLocationRef})
    |]
  where
  renderTimeOfDay (t :: TimeOfDay) = formatTime defaultTimeLocale "TimeOfDay %H %M %S" t

prettyPrintList :: [TimeReference] -> Text
prettyPrintList [] = "Empty list"
prettyPrintList (t : ts) =
  T.unlines $ prettyPrintTimeReference '[' t
    : map (prettyPrintTimeReference ',') ts
    <> ["]"]

assertRefPair :: HasCallStack => TimeReference -> TimeReference -> Assertion
assertRefPair expTr outTr = do
  assertUnit "Text" trText
  assertUnit "Date reference" trDateRef
  assertUnit "Location reference" trLocationRef
  assertUnit "Time of day" trTimeOfDay
  where
    assertUnit :: (Show a, Eq a) => Text -> (TimeReference -> a) -> Assertion
    assertUnit note getter = do
      let expUnit = getter expTr
          outUnit = getter outTr
      when (expUnit /= outUnit) $
        assertFailure [int||#{note}: expected #s{expUnit} but got #s{outUnit}|]
