-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.TzBot.RenderSpec
  ( test_renderSpec
  ) where

import Universum

import Data.List.NonEmpty qualified as NE
import Data.Time (UTCTime)
import Data.Time.TZInfo (TZLabel(..))
import Data.Time.TZTime (toUTC)
import Data.Time.TZTime.QQ (tz)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.Runners (TestTree(..))

import TzBot.Parser (parseWithEmptyContext)
import TzBot.Render
import TzBot.Slack.API

-- Sunday
arbitraryTime1 :: UTCTime
arbitraryTime1 = toUTC [tz|2023-01-30 00:30:00 [Europe/Moscow]|]

nearTimeshift :: UTCTime
nearTimeshift = toUTC [tz|2023-03-11 00:30:00 [Europe/Moscow]|]

userMoscow :: User
userMoscow = User {uId="msk", uIsBot=False, uTz=Europe__Moscow}

userMoscow2 :: User
userMoscow2 = User {uId="msk2", uIsBot=False, uTz=Europe__Moscow}

userHavana :: User
userHavana = User {uId="hav", uIsBot=False, uTz=America__Havana}

test_renderSpec :: TestTree
test_renderSpec = TestGroup "Render"
  [ TestGroup "RenderChat"
    [ testCase "Implicit sender's timezone" $
      mkChatCase arbitraryTime1 "10am" userMoscow userHavana
      [ translWithoutNotes
          "\"10am\", 30 January 2023 in Europe/Moscow"
          "02:00, Monday, 30 January 2023 in America/Havana"
      ]

    , testCase "Implicit day" $
      mkChatCase arbitraryTime1 "10am in Europe/Helsinki" userMoscow userHavana
      [ translWithoutNotes
          "\"10am in Europe/Helsinki\", 30 January 2023"
          "03:00, Monday, 30 January 2023 in America/Havana"
      ]

    , testCase "Everything explicit" $
      mkChatCase arbitraryTime1 "10am in Europe/Helsinki 3 Feb" userMoscow userHavana
      [ translWithoutNotes
          "\"10am in Europe/Helsinki 3 Feb\""
          "03:00, Friday, 03 February 2023 in America/Havana"
      ]
    , testCase "Same timezone" $
      mkChatCase arbitraryTime1 "10am" userMoscow userMoscow2
      [ translWithoutNotes
          "\"10am\", 30 January 2023 in Europe/Moscow"
          "You are in this timezone"
      ]
    , testCase "Back to author, same timezone" $
      mkChatCase arbitraryTime1 "10am" userMoscow userMoscow
      []
    , testCase "Back to author, other timezone" $
      mkChatCase arbitraryTime1 "10am in Europe/Helsinki" userMoscow userMoscow
      [ translWithoutNotes
          "\"10am in Europe/Helsinki\", 30 January 2023"
          "11:00, Monday, 30 January 2023 in Europe/Moscow"
      ]
    , testCase "Implicit timezone & implicit date & explicit weekday" $
      mkChatCase arbitraryTime1 "10am on wednesday" userMoscow userHavana
      [ translWithoutNotes
          "\"10am on wednesday\", 01 February 2023 in Europe/Moscow"
          "02:00, Wednesday, 01 February 2023 in America/Havana"
      ]
    , testCase "Implicit timezone & implicit date & explicit days from today" $
      mkChatCase arbitraryTime1 "10am in 3 days" userMoscow userHavana
      [ translWithoutNotes
          "\"10am in 3 days\", 02 February 2023 in Europe/Moscow"
          "02:00, Thursday, 02 February 2023 in America/Havana"
      ]
    , testCase "Implicit timezone & explicit day & implicit month" $
      mkChatCase arbitraryTime1 "10am on the 21st" userMoscow userHavana
      [ translWithoutNotes
          "\"10am on the 21st\", 21 January 2023 in Europe/Moscow"
          "02:00, Saturday, 21 January 2023 in America/Havana"
      ]
    , testCase "Unknown timezone abbreviation, no similar known ones" $
      mkChatCase arbitraryTime1 "10am KAMAZ" userMoscow userHavana
      [ translWithoutNotes
        "\"10am KAMAZ\""
        "Contains unrecognized timezone abbreviation: KAMAZ"
      ]
    , testCase "Unknown timezone abbreviation, some similar known ones" $
      mkChatCase arbitraryTime1 "10am WETS" userMoscow userHavana
      [ TranslationPair
        "\"10am WETS\""
        "Contains unrecognized timezone abbreviation: WETS"
        (Just "_Maybe you meant: WET, WEST_")
        Nothing
      ]
    , testCase "Known timezone abbreviation" $
      mkChatCase arbitraryTime1 "10am MSK" userMoscow userHavana
      [ translWithoutNotes
          "\"10am MSK\", 30 January 2023, Moscow Time (UTC+03:00) "
          "02:00, Monday, 30 January 2023 in America/Havana"
      ]
    , TestGroup "Overlap"
      [ testCase "Explicit timezone" $
        mkChatCase arbitraryTime1 "0:30 in america/havana on the 6th november" userHavana userMoscow
        [ TranslationPair
          { tuTimeRef = "\"0:30 in america/havana on the 6th november\""
          , tuTranslation = "Ambiguous time"
          , tuNoteForSender = Just "_At 01:00, the clocks are turned backward 1 hour(s) to 00:00 and this particular time occurs twice in America/Havana, first with the offset UTC-04:00 and then with UTC-05:00. Please edit your message or write a new one and specify an offset explicitly._"
          , tuNoteForOthers = Just "_At 01:00, the clocks are turned backward 1 hour(s) to 00:00 and this particular time occurs twice in America/Havana, first with the offset UTC-04:00 and then with UTC-05:00._"
          }
        ]
      , testCase "Implicit timezone" $
        mkChatCase arbitraryTime1 "0:30 on the 6th november" userHavana userMoscow
        [ TranslationPair
          { tuTimeRef = "\"0:30 on the 6th november\" in America/Havana"
          , tuTranslation = "Ambiguous time"
          , tuNoteForSender = Just "_At 01:00, the clocks are turned backward 1 hour(s) to 00:00 and this particular time occurs twice in your timezone (America/Havana), first with the offset UTC-04:00 and then with UTC-05:00. Please edit your message or write a new one and specify an offset explicitly._"
          , tuNoteForOthers = Just "_At 01:00, the clocks are turned backward 1 hour(s) to 00:00 and this particular time occurs twice in the sender's timezone (America/Havana), first with the offset UTC-04:00 and then with UTC-05:00._"
          }
        ]
      ]
    , TestGroup "Gap"
      [ testCase "Explicit timezone" $
        mkChatCase arbitraryTime1 "0:30 in america/havana on the 12th march" userHavana userMoscow
        [ TranslationPair
          { tuTimeRef = "\"0:30 in america/havana on the 12th march\""
          , tuTranslation = "Invalid time"
          , tuNoteForSender = Just "_At 00:00, the clocks are turned forward 1 hour(s) to 01:00 and this particular time does not occur in America/Havana. Please edit your message or write a new one and amend the time. Did you mean 23:30 or 01:30 instead?_"
          , tuNoteForOthers = Just "_At 00:00, the clocks are turned forward 1 hour(s) to 01:00 and this particular time does not occur in America/Havana._"
          }
        ]
      , testCase "Implicit timezone" $
        mkChatCase arbitraryTime1 "0:30 on the 12th march" userHavana userMoscow
        [ TranslationPair
          { tuTimeRef = "\"0:30 on the 12th march\" in America/Havana"
          , tuTranslation = "Invalid time"
          , tuNoteForSender = Just "_At 00:00, the clocks are turned forward 1 hour(s) to 01:00 and this particular time does not occur in your timezone (America/Havana). Please edit your message or write a new one and amend the time. Did you mean 23:30 or 01:30 instead?_"
          , tuNoteForOthers = Just "_At 00:00, the clocks are turned forward 1 hour(s) to 01:00 and this particular time does not occur in the sender's timezone (America/Havana)._"
          }
        ]
      ]
    ]
  , TestGroup "Modal"
    [ testCase "Back to author, same timezone" $
      mkModalCase arbitraryTime1 "10am" userMoscow userMoscow
      [ translWithoutNotes
          "\"10am\", 30 January 2023 in Europe/Moscow"
          "You are in this timezone"
      ]
    ]

  , TestGroup "TimeshiftWarning"
    [ testCase "Implicit date" $
      mkChatCase nearTimeshift "10am" userMoscow userHavana
      [ translWithCommonNote
          "\"10am\", 11 March 2023 in Europe/Moscow"
          "02:00, Saturday, 11 March 2023 in America/Havana"
          "_Warning: We inferred that \"10am\" refers to 11 March 2023 in Europe/Moscow and converted it to America/Havana, but there is a time change near this date_:\n  \8226 _At 00:00, 12 March 2023 in America/Havana, the clocks are turned forward 1 hour(s)_.\n_Beware that if this inference is not correct and the sender meant a different date, the conversion may not be accurate._"
      ]
    , testCase "Day of week" $
      mkChatCase nearTimeshift "10am on sunday" userMoscow userHavana
      [ translWithCommonNote
          "\"10am on sunday\", 12 March 2023 in Europe/Moscow"
          "03:00, Sunday, 12 March 2023 in America/Havana"
          "_Warning: We inferred that \"10am on sunday\" refers to 12 March 2023 in Europe/Moscow and converted it to America/Havana, but there is a time change near this date_:\n  \8226 _At 00:00, 12 March 2023 in America/Havana, the clocks are turned forward 1 hour(s)_.\n_Beware that if this inference is not correct and the sender meant a different date, the conversion may not be accurate._"
      ]
    , testCase "Day of week, inversed users" $
      mkChatCase nearTimeshift "10am on sunday" userHavana userMoscow
      [ translWithCommonNote
          "\"10am on sunday\", 12 March 2023 in America/Havana"
          "17:00, Sunday, 12 March 2023 in Europe/Moscow"
          "_Warning: We inferred that \"10am on sunday\" refers to 12 March 2023 in America/Havana and converted it to Europe/Moscow, but there is a time change near this date_:\n  \8226 _At 00:00, 12 March 2023 in America/Havana, the clocks are turned forward 1 hour(s)_.\n_Beware that if this inference is not correct and the sender meant a different date, the conversion may not be accurate._"
      ]
    , testCase "Timeshifts in both timezones" $
      mkChatCase
        (toUTC [tz|2023-03-25 15:00:00 [Europe/London]|])
        "10am"
        (User {uId="london", uIsBot=False, uTz=Europe__London})
        (User {uId="lisbon", uIsBot=False, uTz=Europe__Lisbon})
      [ translWithCommonNote
          "\"10am\", 26 March 2023 in Europe/London"
          "10:00, Sunday, 26 March 2023 in Europe/Lisbon"
          "_Warning: We inferred that \"10am\" refers to 26 March 2023 in Europe/London and converted it to Europe/Lisbon, but there is a time change near this date_:\n  \8226 _At 01:00, 26 March 2023 in Europe/London, the clocks are turned forward 1 hour(s)_.\n  \8226 _At 01:00, 26 March 2023 in Europe/Lisbon, the clocks are turned forward 1 hour(s)_.\n_Beware that if this inference is not correct and the sender meant a different date, the conversion may not be accurate._"
      ]
    , TestGroup "ExplicitDate"
      [ testCase "No warning: tomorrow" $
        mkChatCase
          (toUTC [tz|2023-03-25 15:00:00 [Europe/London]|])
          "10am tomorrow"
          (User {uId="london", uIsBot=False, uTz=Europe__London})
          (User {uId="lisbon", uIsBot=False, uTz=Europe__Lisbon})
        [ translWithoutNotes
          "\"10am tomorrow\", 26 March 2023 in Europe/London"
          "10:00, Sunday, 26 March 2023 in Europe/Lisbon"
        ]
      , testCase "No warning: 2 days ahead" $
        mkChatCase
          (toUTC [tz|2023-03-24 15:00:00 [Europe/London]|])
          "10am 2 days ahead"
          (User {uId="london", uIsBot=False, uTz=Europe__London})
          (User {uId="lisbon", uIsBot=False, uTz=Europe__Lisbon})
        [ translWithoutNotes
          "\"10am 2 days ahead\", 26 March 2023 in Europe/London"
          "10:00, Sunday, 26 March 2023 in Europe/Lisbon"
        ]
      , testCase "No warning: fully specified date" $
        mkChatCase
          (toUTC [tz|2023-03-24 15:00:00 [Europe/London]|])
          "10am 26 march"
          (User {uId="london", uIsBot=False, uTz=Europe__London})
          (User {uId="lisbon", uIsBot=False, uTz=Europe__Lisbon})
        [ translWithoutNotes
          "\"10am 26 march\" in Europe/London"
          "10:00, Sunday, 26 March 2023 in Europe/Lisbon"
        ]
      ]
    ]
  ]

translWithoutNotes :: Text -> Text -> TranslationPair
translWithoutNotes q w = TranslationPair q w Nothing Nothing

mkModalCase :: UTCTime -> Text -> User -> User -> [TranslationPair] -> Assertion
mkModalCase = mkTestCase asForModalM

mkChatCase :: UTCTime -> Text -> User -> User -> [TranslationPair] -> Assertion
mkChatCase = mkTestCase asForMessageM

translWithCommonNote :: Text -> Text -> Text -> TranslationPair
translWithCommonNote q w e = TranslationPair q w (Just e) (Just e)

mkTestCase :: ModalFlag -> UTCTime -> Text -> User -> User -> [TranslationPair] -> Assertion
mkTestCase modalFlag eventTimestamp refText sender otherUser expectedOtherUserTransl = do
  let [timeRef] = parseWithEmptyContext refText
      ephemeralTemplate =
        renderTemplate modalFlag eventTimestamp sender $
          NE.singleton timeRef

      getTranslationPairs user = renderAllTP user ephemeralTemplate
      otherUserTransl = getTranslationPairs otherUser
  maybe [] toList otherUserTransl @?= expectedOtherUserTransl
