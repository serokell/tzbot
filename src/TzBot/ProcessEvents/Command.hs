-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvents.Command where

import Universum

import Slacker (SlashCommand(..), scChannelId)

import TzBot.RunMonad (BotM(..))
import TzBot.Slack (sendEphemeralMessage)
import TzBot.Slack.API (ChannelId(..), PostEphemeralReq(..), UserId(..))
import TzBot.Slack.Fixtures qualified as Fixtures

processHelpCommand :: SlashCommand -> BotM ()
processHelpCommand SlashCommand {..} = do
  let postEphemeralReq = PostEphemeralReq
        { perUser = UserId scUserId
        , perChannel = ChannelId scChannelId
        , perText = Fixtures.helpMessage
        , perThreadTs = Nothing
        , perBlocks = Nothing
        }
  sendEphemeralMessage postEphemeralReq
