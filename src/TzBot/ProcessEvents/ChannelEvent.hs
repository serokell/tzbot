-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvents.ChannelEvent where

import TzPrelude

import Data.Set qualified as S
import Text.Interpolation.Nyan (int, rmode')

import TzBot.Cache qualified as Cache
import TzBot.Logger
import TzBot.RunMonad
import TzBot.Slack.Events (MemberJoinedChannelEvent(..), MemberLeftChannelEvent(..))

processMemberJoinedChannel :: MemberJoinedChannelEvent -> BotM ()
processMemberJoinedChannel evt = do
  logInfo [int||user #{mjceUser evt} joined channel #{mjceChannel evt}|]
  channelMembersCache <- asks bsConversationMembersCache
  Cache.update evt.mjceChannel (S.insert evt.mjceUser) channelMembersCache

processMemberLeftChannel :: MemberLeftChannelEvent -> BotM ()
processMemberLeftChannel evt = do
  logInfo [int||user #{mlceUser evt} left channel #{mlceChannel evt}|]
  channelMembersCache <- asks bsConversationMembersCache
  Cache.update evt.mlceChannel (S.delete evt.mlceUser) channelMembersCache
