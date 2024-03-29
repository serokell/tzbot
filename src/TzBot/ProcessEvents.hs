-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvents
  ( handler
  ) where

import TzPrelude

import Control.Exception (AsyncException(UserInterrupt))
import Data.Aeson (FromJSON(..), Value)
import Data.Aeson.Types (parseEither)
import Slacker
  (DisconnectBody(DisconnectBody), EventsApiEnvelope(EventsApiEnvelope, eaeEnvelopeId),
  HelloBody(..), SlackConfig, SlashCommandsEnvelope(SlashCommandsEnvelope, sceEnvelopeId),
  SocketModeEvent(..), pattern BlockAction, pattern Command, pattern EventValue,
  pattern Interactive)
import Slacker.SocketMode (InteractiveEnvelope(..))
import Text.Interpolation.Nyan (int, rmode', rmode's)
import UnliftIO.Exception qualified as UnliftIO

import TzBot.Logger
import TzBot.ProcessEvents.BlockAction qualified as B
import TzBot.ProcessEvents.ChannelEvent (processMemberJoinedChannel, processMemberLeftChannel)
import TzBot.ProcessEvents.Command (processHelpCommand)
import TzBot.ProcessEvents.Interactive qualified as I
import TzBot.ProcessEvents.Message (processMessageEvent)
import TzBot.RunMonad (BotM, BotState(..), runBotM)
import TzBot.Slack.API.Block (ActionId(..))
import TzBot.Slack.Events
import TzBot.Slack.Fixtures qualified as Fixtures
import TzBot.Util (encodeText)

{- |
After the message event came, the bot sends some ephemerals
containing conversions of time references in that message.

The bot has two entrypoints, \"view\" and \"report\", that
can be triggered from the message context menu
(see manifest.yml, features/shortcuts).
This causes the `Interactive`/`IEMessageEvent` event to come.

After this, \"view\" or \"report\" dialog window should start.
Both of them contain the original message text and a table of
time references conversions, but the \"view\" window
provides a button \"Report\" that will get the user to
the \"report\" dialog. And instead \"report\" dialog contains
an input block for user to share their thoughts.
When the \"report\" modal is submitted, the `Interactive`/`IEReportViewSubmitted`
event comes, and the bot collects user feedback in the configured way.

The bot also has a command `\tzhelp`, should return help message in response.
 -}
handler :: IORef (IO ()) -> BotState -> SlackConfig -> SocketModeEvent -> IO ()
handler shutdownRef bState _cfg e = run $ do
  logDebug [int||Received Slack event: #{show @Text e}|]
  case e of
    Command cmdType slashCmd -> case cmdType of
      Fixtures.HelpCommand -> katipAddNamespaceText cmdType $ processHelpCommand slashCmd
      unknownCmd           -> logWarn [int||Unknown command #{unknownCmd}|]

    EventValue eventType evtRaw
      | eventType == "message" ->
        decodeAndProcess @MessageEvent eventType envelopeIdentifier processMessageEvent evtRaw
      | eventType == "member_joined_channel" ->
        decodeAndProcess @MemberJoinedChannelEvent eventType envelopeIdentifier processMemberJoinedChannel evtRaw
      | eventType == "member_left_channel" ->
        decodeAndProcess @MemberLeftChannelEvent eventType envelopeIdentifier processMemberLeftChannel evtRaw
      | otherwise -> logWarn [int||Unrecognized EventValue #{encodeText evtRaw}|]

    -- BlockAction events form a subset of Interactive, so check them first
    BlockAction actionId blockActionRaw
      | actionId == unActionId Fixtures.reportButtonActionId ->
        decodeAndProcess @(ViewActionEvent Value) actionId envelopeIdentifier B.processReportButtonToggled blockActionRaw
      | otherwise ->
        logWarn [int||Unrecognized BlockAction #s{e}|]

    Interactive interactiveType interactiveRaw
      | interactiveType == "message_action" ->
        decodeAndProcess @InteractiveMessageEvent interactiveType envelopeIdentifier I.processInteractive interactiveRaw
      | interactiveType == "view_submission" ->
        decodeAndProcess @SubmitViewEvent interactiveType envelopeIdentifier I.processViewSubmission interactiveRaw
      | otherwise ->
        logWarn [int||Unrecognized Interactive event #s{e}|]
    _ -> logWarn [int||Unknown SocketModeEvent #s{e}|]
  where
    run :: BotM a -> IO ()
    run action = void $ runBotM bState $ do
      eithRes <- UnliftIO.trySyncOrAsync action
      whenLeft eithRes $ \e -> do
        case fromException e of
          Just UserInterrupt -> liftIO $ join $ readIORef shutdownRef
          _ -> logError [int||Error occured: #{displayException e}|]

    envelopeIdentifier :: Text
    envelopeIdentifier = case e of
      EventsApi EventsApiEnvelope {..} -> eaeEnvelopeId
      SlashCommands SlashCommandsEnvelope {..} -> sceEnvelopeId
      InteractiveEvent InteractiveEnvelope {..} -> ieEnvelopeId
      Hello HelloBody {} -> "hello_body"
      Disconnect DisconnectBody {} -> "disconnect_body"

decodeAndProcess :: FromJSON a => Text -> Text -> (a -> BotM ()) -> Value -> BotM ()
decodeAndProcess interactiveType envelopeIdentifier processFunc raw = do
  let eithEvt = parseEither parseJSON raw
  case eithEvt of
    Left err -> do
      logError [int||Invalid #{interactiveType} event/action, error #s{err}|]
      logError [int||Full event object: #{encodeText raw}|]
    Right evt -> void $
      katipAddContext (EventContext interactiveType envelopeIdentifier) $
        processFunc evt
