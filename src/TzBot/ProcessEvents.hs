-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvents
  ( handleSlashCommand
  , handleRawEvent
  , handleRawBlockAction
  , handleRawInteractive) where

import Universum

import Data.Aeson (FromJSON(..), Value)
import Data.Aeson.Types (parseEither)
import Slacker (SlashCommand, scCommand)
import Text.Interpolation.Nyan (int, rmode', rmode's)

import TzBot.Logger
import TzBot.ProcessEvents.BlockAction qualified as B
import TzBot.ProcessEvents.ChannelEvent (processMemberJoinedChannel, processMemberLeftChannel)
import TzBot.ProcessEvents.Command (processHelpCommand)
import TzBot.ProcessEvents.Interactive qualified as I
import TzBot.ProcessEvents.Message (processMessageEvent)
import TzBot.RunMonad (BotM)
import TzBot.Slack.API.Block (ActionId(..))
import TzBot.Slack.Fixtures qualified as Fixtures
import TzBot.Util (encodeText)

{-
After the message event came, the bot sends some ephemerals
containing translations of time references in that message.

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

The bot also has a command `\tzhelp`, should return help message in response.
 -}

handleSlashCommand :: SlashCommand -> BotM ()
handleSlashCommand slashCmd = do
  let cmdType = scCommand slashCmd
  case cmdType of
    Fixtures.HelpCommand -> katipAddNamespaceText cmdType $ processHelpCommand slashCmd
    unknownCmd           -> logWarn [int||Unknown command #{unknownCmd}|]

handleRawEvent :: Text -> Text -> Value -> BotM ()
handleRawEvent envelopeIdentifier eventType evtRaw
      | eventType == "message" =
        go processMessageEvent
      | eventType == "member_joined_channel" =
        go processMemberJoinedChannel
      | eventType == "member_left_channel" =
        go processMemberLeftChannel
      | otherwise = logWarn [int||Unrecognized EventValue #{encodeText evtRaw}|]
  where
    go :: (FromJSON a) => (a -> BotM ()) -> BotM ()
    go action = decodeAndProcess eventType envelopeIdentifier action evtRaw

-- BlockAction events form a subset of Interactive, so check them first
handleRawBlockAction :: Text -> Text -> Value -> BotM ()
handleRawBlockAction envelopeIdentifier actionId blockActionRaw
  | actionId == unActionId Fixtures.reportButtonActionId =
    decodeAndProcess actionId envelopeIdentifier B.processReportButtonToggled blockActionRaw
  | otherwise =
    logWarn [int||Unrecognized BlockAction identifier #{actionId}|]

handleRawInteractive :: Text -> Text -> Value -> BotM ()
handleRawInteractive envelopeIdentifier interactiveType interactiveRaw
  | interactiveType == "message_action" =
    decodeAndProcess interactiveType envelopeIdentifier I.processInteractive interactiveRaw
  | interactiveType == "view_submission" =
    decodeAndProcess interactiveType envelopeIdentifier I.processViewSubmission interactiveRaw
  | otherwise =
    logWarn [int||Unrecognized Interactive event type #{interactiveType}|]

decodeAndProcess :: FromJSON a => Text -> Text -> (a -> BotM b) -> Value -> BotM ()
decodeAndProcess interactiveType envelopeIdentifier processFunc raw = do
  let eithEvt = parseEither parseJSON raw
  case eithEvt of
    Left err -> do
      logError [int||Invalid #{interactiveType} event/action, error #s{err}|]
      logError [int||Full event object: #{encodeText raw}|]
    Right evt -> void $
      katipAddContext (EventContext interactiveType envelopeIdentifier) $
        processFunc evt
