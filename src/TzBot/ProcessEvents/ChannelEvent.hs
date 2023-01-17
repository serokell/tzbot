-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvents.ChannelEvent where

import Universum

import Data.Set qualified as S
import Text.Interpolation.Nyan (int, rmode')

import TzBot.Cache qualified as Cache
import TzBot.Logger (info, logTM)
import TzBot.RunMonad
import TzBot.Slack.Events (MemberJoinedChannelEvent(..), MemberLeftChannelEvent(..))

processMemberJoinedChannel :: MemberJoinedChannelEvent -> BotM ()
processMemberJoinedChannel MemberJoinedChannelEvent {..} = do
  $(logTM) `info` [int||user #{mjceUser} joined channel #{mjceChannel}|]
  channelMembersCache <- asks bsConversationMembersCache
  Cache.update mjceChannel (S.insert mjceUser) channelMembersCache

processMemberLeftChannel :: MemberLeftChannelEvent -> BotM ()
processMemberLeftChannel MemberLeftChannelEvent {..} = do
  $(logTM) `info` [int||user #{mlceUser} left channel #{mlceChannel}|]
  channelMembersCache <- asks bsConversationMembersCache
  Cache.update mlceChannel (S.delete mlceUser) channelMembersCache
