-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.TzBot.RenderSpec
  ( test_renderSpec
  ) where

import Universum

import Data.List (singleton)
import Data.List.NonEmpty qualified as NE
import Data.Time (UTCTime)
import Data.Time.TZInfo (TZLabel(..))
import Data.Time.TZTime (toUTC)
import Data.Time.TZTime.QQ (tz)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.Runners (TestTree(..))
import Text.Interpolation.Nyan
import TzBot.Parser (parseTimeRefs)
import TzBot.Render
import TzBot.Slack.API (Block(..), Mrkdwn(..), User(..), fieldsSection, markdownSection)

-- Sunday
arbitraryTime1 :: UTCTime
arbitraryTime1 = toUTC [tz|2023-01-30 00:30:00 [Europe/Moscow]|]

arbitraryTime2 :: UTCTime
arbitraryTime2 = toUTC [tz|2023-01-30 01:30:00 [Europe/Moscow]|]

havanaTime1 :: UTCTime
havanaTime1 = toUTC [tz|2023-03-11 01:30:00 [America/Havana]|]

havanaTime2 :: UTCTime
havanaTime2 = toUTC [tz|2022-11-05 01:30:00 [America/Havana]|]

userMoscow :: User
userMoscow = User {uId="msk1", uIsBot=False, uTz=Europe__Moscow}

userHavana :: User
userHavana = User {uId="hav", uIsBot=False, uTz=America__Havana}

test_renderSpec :: TestTree
test_renderSpec = TestGroup "Render" $
  [ testCase "Implicit sender's timezone" $ mkTestCase arbitraryTime1 "10am" userMoscow userHavana

      []
      (mkBlocks "\"10am\" in Europe/Moscow:" "*02:00 in America/Havana* (possibly, Monday, January 30)")

  , testCase "Inferred today" $ mkTestCase arbitraryTime1 "10am in Europe/Helsinki" userMoscow userHavana

      (mkBlocks "\"10am in Europe/Helsinki\":" "*11:00 in Europe/Moscow* (possibly, Monday, January 30)")
      (mkBlocks "\"10am in Europe/Helsinki\":" "*03:00 in America/Havana* (possibly, Monday, January 30)")

  , testCase "Same timezone" $ mkTestCase arbitraryTime1 "10am in America/Havana" userMoscow userHavana

      (mkBlocks "\"10am in America/Havana\":" "*18:00 in Europe/Moscow* (possibly, Monday, January 30)")
      (mkBlocks "\"10am in America/Havana\":" "*your timezone is the same*")

  , testCase "Close day of week" $ mkTestCase arbitraryTime1 "10am in Europe/Helsinki tuesday" userMoscow userHavana

      (mkBlocks "\"10am in Europe/Helsinki tuesday\":" "*11:00 in Europe/Moscow, Tuesday, January 31*")
      (mkBlocks "\"10am in Europe/Helsinki tuesday\":" "*03:00 in America/Havana, Tuesday, January 31*")

  , testCase "Far day of week" $ mkTestCase arbitraryTime1 "10am in Europe/Helsinki thursday" userMoscow userHavana

      (mkBlocks "\"10am in Europe/Helsinki thursday\":" "*11:00 in Europe/Moscow* (possibly, Thursday, February 02)")
      (mkBlocks "\"10am in Europe/Helsinki thursday\":" "*03:00 in America/Havana* (possibly, Thursday, February 02)")

  , testCase "Relative ref, unsure" $ mkTestCase arbitraryTime1 "10am in Europe/Helsinki tomorrow" userMoscow userHavana
      -- not sure because it's already new day in Moscow
      (mkBlocks "\"10am in Europe/Helsinki tomorrow\":" "*11:00 in Europe/Moscow* (possibly, Monday, January 30)")
      (mkBlocks "\"10am in Europe/Helsinki tomorrow\":" "*03:00 in America/Havana* (possibly, Monday, January 30)")

  , testCase "Relative ref, sure" $ mkTestCase arbitraryTime2 "10am in Europe/Helsinki tomorrow" userMoscow userHavana

      (mkBlocks "\"10am in Europe/Helsinki tomorrow\":" "*11:00 in Europe/Moscow, Tuesday, January 31*")
      (mkBlocks "\"10am in Europe/Helsinki tomorrow\":" "*03:00 in America/Havana, Tuesday, January 31*")

  , testCase "Only day of month" $ mkTestCase arbitraryTime2 "10am in Europe/Helsinki on 3rd" userMoscow userHavana
      -- not sure
      (mkBlocks "\"10am in Europe/Helsinki on 3rd\":" "*11:00 in Europe/Moscow* (possibly, Friday, February 03)")
      (mkBlocks "\"10am in Europe/Helsinki on 3rd\":" "*03:00 in America/Havana* (possibly, Friday, February 03)")

  , testCase "Full date" $ mkTestCase arbitraryTime2 "10am in Europe/Helsinki 3rd February" userMoscow userHavana
      -- sure
      (mkBlocks "\"10am in Europe/Helsinki 3rd February\":" "*11:00 in Europe/Moscow, Friday, February 03*")
      (mkBlocks "\"10am in Europe/Helsinki 3rd February\":" "*03:00 in America/Havana, Friday, February 03*")

  , TestGroup "Invalid date"
    [ testCase "Implicit timezone" $ mkTestCase havanaTime1 "0:30 tomorrow" userHavana userMoscow
      (mkBlocksWithNote
        [int||"0:30 tomorrow" in America/Havana:|]
        [int||*invalid because of the time shift*|]
        [int|n|_There is a timeshift in your timezone (America/Havana) around the
        specified time and this particular timestamp does not exist, please \
        define the offset explicitly._|])
      (mkBlocksWithNote
        [int||"0:30 tomorrow" in America/Havana:|]
        [int||*invalid because of the time shift*|]
        [int|n|_There is a time zone shift in the sender's timezone (America/Havana)
        around that time, and this particular timestamp does not exist._|])
    , testCase "Explicit timezone" $ mkTestCase havanaTime1 "0:30 in America/Havana tomorrow" userHavana userMoscow
      (mkBlocksWithNote
        [int||"0:30 in America/Havana tomorrow":|]
        [int||*invalid because of the time shift*|]
        [int|n|_There is a timeshift in America/Havana around the
        specified time and this particular timestamp does not exist, please \
        define the offset explicitly._|])
      (mkBlocksWithNote
        [int||"0:30 in America/Havana tomorrow":|]
        [int||*invalid because of the time shift*|]
        [int|n|_There is a time zone shift in America/Havana
        around that time, and this particular timestamp does not exist._|])
    ]

  , TestGroup "Ambiguous date"
    [ testCase "Implicit timezone" $ mkTestCase havanaTime2 "0:30 tomorrow" userHavana userMoscow
      (mkBlocksWithNote
        [int||"0:30 tomorrow" in America/Havana:|]
        [int||*ambiguous because of the time shift*|]
        [int|n|_There is a timeshift in your timezone (America/Havana) around the
        specified time and this particular timestamp can be possible with
        different offsets, please define the offset explicitly._|])
      (mkBlocksWithNote
        [int||"0:30 tomorrow" in America/Havana:|]
        [int||*ambiguous because of the time shift*|]
        [int|n|_There is a time zone shift in the sender's timezone (America/Havana)
        around that time, and this particular timestamp
        can be possible with different offsets._|])
    , testCase "Explicit timezone" $ mkTestCase havanaTime2 "0:30 in America/Havana tomorrow" userHavana userMoscow
      (mkBlocksWithNote
        [int||"0:30 in America/Havana tomorrow":|]
        [int||*ambiguous because of the time shift*|]
        [int|n|_There is a timeshift in America/Havana around the
        specified time and this particular timestamp can be possible with
        different offsets, please define the offset explicitly._|])
      (mkBlocksWithNote
        [int||"0:30 in America/Havana tomorrow":|]
        [int||*ambiguous because of the time shift*|]
        [int|n|_There is a time zone shift in America/Havana
        around that time, and this particular timestamp
        can be possible with different offsets._|])
    ]
  ]

mkBlocks :: Text -> Text -> [Block]
mkBlocks timeRef translation =
  singleton $ BSection $ (fieldsSection Nothing Nothing $ NE.singleton (Mrkdwn timeRef, Mrkdwn translation))

mkBlocksWithNote :: Text -> Text -> Text -> [Block]
mkBlocksWithNote timeRef translation note =
  [ BSection $ (fieldsSection Nothing Nothing $ NE.singleton (Mrkdwn timeRef, Mrkdwn translation))
  , BSection $ flip markdownSection Nothing $ Mrkdwn note
  ]

mkTestCase :: UTCTime -> Text -> User -> User -> [Block] -> [Block] -> Assertion
mkTestCase eventTimestamp refText sender otherUser expectedSenderBlocks expectedOtherUserBlocks = do
  let [timeRef] = parseTimeRefs refText
      ephemeralTemplate =
        renderTemplate asForMessageM eventTimestamp sender $
          NE.singleton timeRef

      getBlocks senderFlag user = maybe [] (renderSlackBlocks senderFlag . Just) $ renderAllTP user ephemeralTemplate
      senderBlocks = getBlocks asForSenderS sender
      otherUserBlocks = getBlocks asForOthersS otherUser
  senderBlocks @?= expectedSenderBlocks
  otherUserBlocks @?= expectedOtherUserBlocks
