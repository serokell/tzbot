-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvents.Command where

import TzPrelude

import Slacker (SlashCommand(..), scChannelId)
import Text.Interpolation.Nyan (int, rmode')

import TzBot.Logger
import TzBot.RunMonad (BotM(..))
import TzBot.Slack (sendEphemeralMessage)
import TzBot.Slack.API (ChannelId(..), PostEphemeralReq(..), UserId(..))
import TzBot.Slack.Fixtures qualified as Fixtures

processHelpCommand :: SlashCommand -> BotM ()
processHelpCommand cmd = do
  let postEphemeralReq = PostEphemeralReq
        { perUser = UserId cmd.scUserId
        , perChannel = ChannelId cmd.scChannelId
        , perText = Fixtures.helpMessage
        , perThreadTs = Nothing
        , perBlocks = Nothing
        }
  logInfo [int||Sending help message to user #{perUser postEphemeralReq}|]
  sendEphemeralMessage postEphemeralReq
