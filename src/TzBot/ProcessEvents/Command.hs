-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvents.Command where

import Universum

import Slacker (SlashCommand(..), scChannelId)
import Text.Interpolation.Nyan (int, rmode')

import TzBot.Logger (info, logTM)
import TzBot.RunMonad (BotM(..))
import TzBot.Slack (sendEphemeralMessage)
import TzBot.Slack.API (ChannelId(..), PostEphemeralReq(..), UserId(..))
import TzBot.Slack.Fixtures qualified as Fixtures

processHelpCommand :: SlashCommand -> BotM ()
processHelpCommand SlashCommand {..} = do
  let postEphemeralReq@PostEphemeralReq {..} = PostEphemeralReq
        { perUser = UserId scUserId
        , perChannel = ChannelId scChannelId
        , perText = Fixtures.helpMessage
        , perThreadTs = Nothing
        , perBlocks = Nothing
        }
  $(logTM) `info` [int||Sending help message to user #{perUser}|]
  sendEphemeralMessage postEphemeralReq
