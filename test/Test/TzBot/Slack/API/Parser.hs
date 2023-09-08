-- SPDX-FileCopyrightText: 2023 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.TzBot.Slack.API.Parser
  ( unit_Parse_normal_message
  , unit_Parse_message_channel_join_events
  , unit_Parse_message_edited
  , unit_Parse_message_with_broadcast
  ) where

import TzPrelude

import Data.Aeson
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Aeson.Types (parseEither)
import Test.Tasty.HUnit
import Text.Read (read)

import TzBot.Slack.API
import TzBot.Slack.API.MessageBlock
import TzBot.Slack.Events
import TzBot.Util

-- | These events happen when a user sends a message to a channel
unit_Parse_normal_message :: Assertion
unit_Parse_normal_message = do
  parseEither @_ @MessageEvent parseJSON [aesonQQ|
  {
      "blocks": [
          {
              "block_id": "gcG",
              "elements": [
                  {
                      "elements": [
                          {
                              "text": "This is a normal message",
                              "type": "text"
                          }
                      ],
                      "type": "rich_text_section"
                  }
              ],
              "type": "rich_text"
          }
      ],
      "channel": "C02N85E82LV",
      "channel_type": "channel",
      "client_msg_id": "9f4b9529-f19f-4587-9b12-a412fe36d070",
      "event_ts": "1694191319.871489",
      "team": "T02NDBHSWSG",
      "text": "This is a normal message",
      "ts": "1694191319.871489",
      "type": "message",
      "user": "U02N85E78QM"
  }
  |] @?=
    Right
      ( MessageEvent
          { meChannel = ChannelId
              { unChannelId = "C02N85E82LV" }
          , meChannelType = Just CTChannel
          , meMessage = Message
              { mUser = UserId
                  { unUserId = "U02N85E78QM" }
              , mText = "This is a normal message"
              , mMessageId = MessageId
                  { unMessageId = "1694191319.871489" }
              , mTs = read "2023-09-08 16:41:59.871489 UTC"
              , mThreadId = Nothing
              , mEdited = False
              , mSubType = Nothing
              , msgBlocks = Just
                  ( WithUnknown
                      { unUnknown = Right
                          [ MessageBlock
                              { mbElements =
                                  [ BEL1Plain
                                      ( PlainBlockElementLevel1
                                          { beType = WithUnknown { unUnknown = Right BETRichTextSection }
                                          , beElements = Just
                                              [ WithUnknown
                                                  { unUnknown = Right
                                                      ( BEL2ElementText
                                                          ( ElementText
                                                              { etText = "This is a normal message"
                                                              , etStyle = Nothing
                                                              }
                                                          )
                                                      )
                                                  }
                                              ]
                                          }
                                      )
                                  ]
                              }
                          ]
                      }
                  )
              }
          , meTs = read "2023-09-08 16:41:59.871489 UTC"
          , meMessageDetails = MDMessage
          }
      )

-- | These events happen when a user joins a channel
-- and Slack displays a grey message like "John joined the channel".
--
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

-- | These events happen when a user sends a message to a channel and then edits it.
unit_Parse_message_edited :: Assertion
unit_Parse_message_edited =
  parseEither @_ @MessageEvent parseJSON [aesonQQ|
  {
      "channel": "C02N85E82LV",
      "channel_type": "channel",
      "event_ts": "1694188505.002400",
      "hidden": true,
      "message": {
          "blocks": [
              {
                  "block_id": "NAiY",
                  "elements": [
                      {
                          "elements": [
                              {
                                  "text": "BBB",
                                  "type": "text"
                              }
                          ],
                          "type": "rich_text_section"
                      }
                  ],
                  "type": "rich_text"
              }
          ],
          "client_msg_id": "f1d9ac1d-250f-494b-8906-83e61210d3db",
          "edited": {
              "ts": "1694188505.000000",
              "user": "U02N85E78QM"
          },
          "source_team": "T02NDBHSWSG",
          "team": "T02NDBHSWSG",
          "text": "BBB",
          "ts": "1694188495.898759",
          "type": "message",
          "user": "U02N85E78QM",
          "user_team": "T02NDBHSWSG"
      },
      "previous_message": {
          "blocks": [
              {
                  "block_id": "gVzT",
                  "elements": [
                      {
                          "elements": [
                              {
                                  "text": "AAA",
                                  "type": "text"
                              }
                          ],
                          "type": "rich_text_section"
                      }
                  ],
                  "type": "rich_text"
              }
          ],
          "client_msg_id": "f1d9ac1d-250f-494b-8906-83e61210d3db",
          "team": "T02NDBHSWSG",
          "text": "AAA",
          "ts": "1694188495.898759",
          "type": "message",
          "user": "U02N85E78QM"
      },
      "subtype": "message_changed",
      "ts": "1694188505.002400",
      "type": "message"
  }
  |] @?= Right
    ( MessageEvent
        { meChannel = ChannelId
            { unChannelId = "C02N85E82LV" }
        , meChannelType = Just CTChannel
        , meMessage = Message
            { mUser = UserId
                { unUserId = "U02N85E78QM" }
            , mText = "BBB"
            , mMessageId = MessageId
                { unMessageId = "1694188495.898759" }
            , mTs = read "2023-09-08 15:54:55.898759 UTC"
            , mThreadId = Nothing
            , mEdited = True
            , mSubType = Nothing
            , msgBlocks = Just
                ( WithUnknown
                    { unUnknown = Right
                        [ MessageBlock
                            { mbElements =
                                [ BEL1Plain
                                    ( PlainBlockElementLevel1
                                        { beType = WithUnknown { unUnknown = Right BETRichTextSection }
                                        , beElements = Just
                                            [ WithUnknown
                                                { unUnknown = Right
                                                    ( BEL2ElementText
                                                        ( ElementText
                                                            { etText = "BBB"
                                                            , etStyle = Nothing
                                                            }
                                                        )
                                                    )
                                                }
                                            ]
                                        }
                                    )
                                ]
                            }
                        ]
                    }
                )
            }
        , meTs = read "2023-09-08 15:55:05.0024 UTC"
        , meMessageDetails = MDMessageEdited
            ( Message
                { mUser = UserId
                    { unUserId = "U02N85E78QM" }
                , mText = "AAA"
                , mMessageId = MessageId
                    { unMessageId = "1694188495.898759" }
                , mTs = read "2023-09-08 15:54:55.898759 UTC"
                , mThreadId = Nothing
                , mEdited = False
                , mSubType = Nothing
                , msgBlocks = Just
                    ( WithUnknown
                        { unUnknown = Right
                            [ MessageBlock
                                { mbElements =
                                    [ BEL1Plain
                                        ( PlainBlockElementLevel1
                                            { beType = WithUnknown { unUnknown = Right BETRichTextSection }
                                            , beElements = Just
                                                [ WithUnknown
                                                    { unUnknown = Right
                                                        ( BEL2ElementText
                                                            ( ElementText
                                                                { etText = "AAA"
                                                                , etStyle = Nothing
                                                                }
                                                            )
                                                        )
                                                    }
                                                ]
                                            }
                                        )
                                    ]
                                }
                            ]
                        }
                    )
                }
            )
        }
    )

-- | These events happen when a user replies in a thread
-- and checks the "Also send to #channel" checkbox.
unit_Parse_message_with_broadcast :: Assertion
unit_Parse_message_with_broadcast =
  parseEither @_ @MessageEvent parseJSON [aesonQQ|
  {
      "channel": "C02N85E82LV",
      "channel_type": "channel",
      "event_ts": "1694190804.002600",
      "hidden": true,
      "message": {
          "blocks": [
              {
                  "block_id": "kke",
                  "elements": [
                      {
                          "elements": [
                              {
                                  "text": "This is a message with broadcast",
                                  "type": "text"
                              }
                          ],
                          "type": "rich_text_section"
                      }
                  ],
                  "type": "rich_text"
              }
          ],
          "client_msg_id": "203315b8-f848-475c-b0b5-da0db4acd993",
          "root": {
              "blocks": [
                  {
                      "block_id": "Sr5",
                      "elements": [
                          {
                              "elements": [
                                  {
                                      "text": "First message",
                                      "type": "text"
                                  }
                              ],
                              "type": "rich_text_section"
                          }
                      ],
                      "type": "rich_text"
                  }
              ],
              "client_msg_id": "2e3a25aa-4b9e-4c78-bef0-be3b8656a53d",
              "is_locked": false,
              "latest_reply": "1694190803.567119",
              "reply_count": 1,
              "reply_users": [
                  "U02N85E78QM"
              ],
              "reply_users_count": 1,
              "team": "T02NDBHSWSG",
              "text": "First message",
              "thread_ts": "1694190770.058699",
              "ts": "1694190770.058699",
              "type": "message",
              "user": "U02N85E78QM"
          },
          "subtype": "thread_broadcast",
          "text": "This is a message with broadcast",
          "thread_ts": "1694190770.058699",
          "ts": "1694190803.567119",
          "type": "message",
          "user": "U02N85E78QM"
      },
      "previous_message": {
          "blocks": [
              {
                  "block_id": "kke",
                  "elements": [
                      {
                          "elements": [
                              {
                                  "text": "This is a message with broadcast",
                                  "type": "text"
                              }
                          ],
                          "type": "rich_text_section"
                      }
                  ],
                  "type": "rich_text"
              }
          ],
          "client_msg_id": "203315b8-f848-475c-b0b5-da0db4acd993",
          "root": {
              "blocks": [
                  {
                      "block_id": "Sr5",
                      "elements": [
                          {
                              "elements": [
                                  {
                                      "text": "First message",
                                      "type": "text"
                                  }
                              ],
                              "type": "rich_text_section"
                          }
                      ],
                      "type": "rich_text"
                  }
              ],
              "client_msg_id": "2e3a25aa-4b9e-4c78-bef0-be3b8656a53d",
              "is_locked": false,
              "latest_reply": "1694190803.567119",
              "reply_count": 1,
              "reply_users": [
                  "U02N85E78QM"
              ],
              "reply_users_count": 1,
              "team": "T02NDBHSWSG",
              "text": "First message",
              "thread_ts": "1694190770.058699",
              "ts": "1694190770.058699",
              "type": "message",
              "user": "U02N85E78QM"
          },
          "subtype": "thread_broadcast",
          "text": "This is a message with broadcast",
          "thread_ts": "1694190770.058699",
          "ts": "1694190803.567119",
          "type": "message",
          "user": "U02N85E78QM"
      },
      "subtype": "message_changed",
      "ts": "1694190804.002600",
      "type": "message"
  }
  |] @?= Right
    ( MessageEvent
        { meChannel = ChannelId
            { unChannelId = "C02N85E82LV" }
        , meChannelType = Just CTChannel
        , meMessage = Message
            { mUser = UserId
                { unUserId = "U02N85E78QM" }
            , mText = "This is a message with broadcast"
            , mMessageId = MessageId
                { unMessageId = "1694190803.567119" }
            , mTs = read "2023-09-08 16:33:23.567119 UTC"
            , mThreadId = Just
                ( ThreadId
                    { unThreadId = "1694190770.058699" }
                )
            , mEdited = False
            , mSubType = Just "thread_broadcast"
            , msgBlocks = Just
                ( WithUnknown
                    { unUnknown = Right
                        [ MessageBlock
                            { mbElements =
                                [ BEL1Plain
                                    ( PlainBlockElementLevel1
                                        { beType = WithUnknown { unUnknown = Right BETRichTextSection }
                                        , beElements = Just
                                            [ WithUnknown
                                                { unUnknown = Right
                                                    ( BEL2ElementText
                                                        ( ElementText
                                                            { etText = "This is a message with broadcast"
                                                            , etStyle = Nothing
                                                            }
                                                        )
                                                    )
                                                }
                                            ]
                                        }
                                    )
                                ]
                            }
                        ]
                    }
                )
            }
        , meTs = read "2023-09-08 16:33:24.0026 UTC"
        , meMessageDetails = MDMessageBroadcast
        }
    )
