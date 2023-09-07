-- SPDX-FileCopyrightText: 2023 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.TzBot.Slack.API.Parser
  ( unit_Parse_message_channel_join_events
  ) where

import TzPrelude

import Data.Aeson
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Aeson.Types (parseEither)

import Test.Tasty.HUnit
import Text.Read (read)
import TzBot.Slack.API
import TzBot.Slack.Events

-- https://github.com/serokell/tzbot/issues/107
unit_Parse_message_channel_join_events :: Assertion
unit_Parse_message_channel_join_events = do
  parseEither @_ @MessageEvent parseJSON [aesonQQ|
    {
        "channel": "CKL24PSG4",
        "channel_type": "channel",
        "event_ts": "1693995287.039729",
        "inviter": "UCEDSC6AK",
        "subtype": "channel_join",
        "text": "<@U04NKKJ4JEN> has joined the channel",
        "ts": "1693995287.039729",
        "type": "message",
        "user": "U04NKKJ4JEN"
    }
  |] @?=
    Right
      ( MessageEvent
        { meChannel = ChannelId
            { unChannelId = "CKL24PSG4" }
        , meChannelType = Just CTChannel
        , meMessage = Message
            { mUser = UserId
                { unUserId = "U04NKKJ4JEN" }
            , mText = "<@U04NKKJ4JEN> has joined the channel"
            , mMessageId = MessageId
                { unMessageId = "1693995287.039729" }
            , mTs = read "2023-09-06 10:14:47.039729 UTC"
            , mThreadId = Nothing
            , mEdited = False
            , mSubType = Just "channel_join"
            , msgBlocks = Nothing
            }
        , meTs = read "2023-09-06 10:14:47.039729 UTC"
        , meMessageDetails = MDUserJoinedChannel
        }
      )
