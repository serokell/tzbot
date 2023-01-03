-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvents
  ( handler
  ) where

import Universum

import Data.Aeson (FromJSON(..), Value)
import Data.Aeson.Types (parseEither)
import Slacker
  (SlackConfig, SocketModeEvent(..), pattern BlockAction, pattern EventValue, pattern Interactive)
import Text.Interpolation.Nyan (int, rmode', rmode's)

import TzBot.ProcessEvents.BlockAction qualified as B
import TzBot.ProcessEvents.ChannelEvent (processMemberJoinedChannel, processMemberLeftChannel)
import TzBot.ProcessEvents.Interactive qualified as I
import TzBot.ProcessEvents.Message (processMessageEvent)
import TzBot.RunMonad (BotM, BotState, log', runBotM)
import TzBot.Slack.API.Block (ActionId(..))
import TzBot.Slack.Fixtures qualified as Fixtures
import TzBot.Util (encodeText)

{- |
After the message event came, the bot sends some ephemerals
containing translations of time references in that message.
Also the ephemeral contains the \"Report\" button that
user can click if something is processed wrong, then
the \"report\" dialog will start (see below). On this clicking,
the `BlockAction`/`BAReportButtonFromEphemeral` event comes
(see "ProcessEvents.BlockAction").

The bot has two entrypoints, \"view\" and \"report\", that
can be triggered from the message context menu
(see manifest.yml, features/shortcuts).
This causes the `Interactive`/`IEMessageEvent` event to come.

After this, \"view\" or \"report\" dialog window should start.
Both of them contain the original message text and a table of
time references translations, but the \"view\" window
provides a button \"Report\" that will get the user to
the \"report\" dialog. And instead \"report\" dialog contains
an input block for user to share their thoughts.
When the \"report\" modal is submitted, the `Interactive`/`IEReportViewSubmitted`
event comes, and the bot collects user feedback in the configured way.
 -}
handler :: BotState -> SlackConfig -> SocketModeEvent -> IO ()
handler wstate _cfg = \e -> do
  run $ case e of
    EventValue eventType evtRaw
      | eventType == "message" ->
        decodeAndProcess eventType processMessageEvent evtRaw
      | eventType == "member_joined_channel" ->
        decodeAndProcess eventType processMemberJoinedChannel evtRaw
      | eventType == "member_left_channel" ->
        decodeAndProcess eventType processMemberLeftChannel evtRaw
      | otherwise -> log' [int||Unrecognized EventValue #{encodeText evtRaw}|]

  -- BlockAction events form a subset of Interactive, so check them first
    BlockAction actionId blockActionRaw
      | actionId == unActionId Fixtures.reportButtonActionId ->
        decodeAndProcess actionId B.processReportButtonToggled blockActionRaw
      | Just messageId <-
          Fixtures.getMessageIdFromReportEphemeral $ ActionId actionId ->
        decodeAndProcess
          actionId
          (\x -> B.processReportButtonFromEphemeral (x, messageId))
          blockActionRaw
      | otherwise ->
        log' [int||Unrecognized BlockAction #s{e}|]

    Interactive interactiveType interactiveRaw
      | interactiveType == "message_action" ->
        decodeAndProcess interactiveType I.processInteractive interactiveRaw
      | interactiveType == "view_submission" ->
        decodeAndProcess interactiveType I.processViewSubmission interactiveRaw
      | otherwise ->
        log' [int||Unrecognized Interactive event #s{e}|]
    _ -> log' [int||Unknown SocketModeEvent #s{e}|]
  where
    run :: BotM a -> IO ()
    run action = do
      eithRes <- runBotM wstate action
      case eithRes of
        Left err -> log' [int||Error occured: #s{err}|]
        Right _ -> pure ()

decodeAndProcess :: FromJSON a => Text -> (a -> BotM b) -> Value -> BotM ()
decodeAndProcess interactiveType processFunc raw = do
  let eithEvt = parseEither parseJSON raw
  case eithEvt of
    Left err -> do
      log' [int||Invalid #{interactiveType} event/action, error #s{err}|]
      log' [int||Full event object: #{encodeText raw}|]
    Right evt -> void $ processFunc evt
