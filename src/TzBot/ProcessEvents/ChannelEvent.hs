-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvents.ChannelEvent where

import Universum

import Data.Set qualified as S
import Text.Interpolation.Nyan (int, rmode')

import TzBot.Cache qualified as Cache
import TzBot.RunMonad
import TzBot.Slack.Events (MemberJoinedChannelEvent(..), MemberLeftChannelEvent(..))

processMemberJoinedChannel :: MemberJoinedChannelEvent -> BotM ()
processMemberJoinedChannel MemberJoinedChannelEvent {..} = do
  log' [int||member_joined_channel: \
               the user #{mjceUser} joined the channel #{mjceChannel}|]
  channelMembersCache <- asks bsConversationMembersCache
  Cache.update mjceChannel (S.insert mjceUser) channelMembersCache

processMemberLeftChannel :: MemberLeftChannelEvent -> BotM ()
processMemberLeftChannel MemberLeftChannelEvent {..} = do
  log' [int||member_left_channel: \
               the user #{mlceUser} left the channel #{mlceChannel}|]
  channelMembersCache <- asks bsConversationMembersCache
  Cache.update mlceChannel (S.delete mlceUser) channelMembersCache
