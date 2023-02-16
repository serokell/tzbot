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
import TzBot.Parser (parseTimeRefs)
import TzBot.Render
import TzBot.Slack.API (User(..))

-- Sunday
arbitraryTime1 :: UTCTime
arbitraryTime1 = toUTC [tz|2023-01-30 00:30:00 [Europe/Moscow]|]

userMoscow :: User
userMoscow = User {uId="msk1", uIsBot=False, uTz=Europe__Moscow}

userMoscow2 :: User
userMoscow2 = User {uId="msk2", uIsBot=False, uTz=Europe__Moscow}

userHavana :: User
userHavana = User {uId="hav", uIsBot=False, uTz=America__Havana}

test_renderSpec :: TestTree
test_renderSpec = TestGroup "Render"
  [ TestGroup "RenderChar"
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
    ]
  , TestGroup "Modal"
    [ testCase "Back to author, same timezone" $
      mkModalCase arbitraryTime1 "10am" userMoscow userMoscow
      [ translWithoutNotes
          "\"10am\", 30 January 2023 in Europe/Moscow"
          "You are in this timezone"
      ]
    ]
  ]

translWithoutNotes :: Text -> Text -> TranslationPair
translWithoutNotes q w = TranslationPair q w Nothing Nothing

mkModalCase :: UTCTime -> Text -> User -> User -> [TranslationPair] -> Assertion
mkModalCase = mkTestCase asForModalM

mkChatCase :: UTCTime -> Text -> User -> User -> [TranslationPair] -> Assertion
mkChatCase = mkTestCase asForMessageM

mkTestCase :: ModalFlag -> UTCTime -> Text -> User -> User -> [TranslationPair] -> Assertion
mkTestCase modalFlag eventTimestamp refText sender otherUser expectedOtherUserTransl = do
  let [timeRef] = parseTimeRefs refText
      ephemeralTemplate =
        renderTemplate modalFlag eventTimestamp sender $
          NE.singleton timeRef

      getTranslationPairs user = renderAllTP user ephemeralTemplate
      otherUserTransl = getTranslationPairs otherUser
  maybe [] toList otherUserTransl @?= expectedOtherUserTransl
