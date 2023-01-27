-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Slack.Fixtures
  ( -- * Entrypoints callback_ids
    pattern ViewEntrypointCallbackId
  , pattern ReportEntrypointCallbackId

  -- * Block action_ids
  , reportButtonActionId
  , reportButtonBlockId
  , ReportInputElementActionId
  , ReportInputBlockId
  , reportInputElementActionId
  , reportInputBlockId

  -- * Modal identifiers
  , viewModal
  , reportModal

  -- * Help message
  , pattern HelpCommand
  , helpMessage
  , helpUsage
  ) where

import Universum

import GHC.TypeLits (symbolVal)

import Text.Interpolation.Nyan (int, rmode')
import TzBot.Slack.API (ActionId(..), BlockId)

pattern ViewEntrypointCallbackId :: Text
pattern ViewEntrypointCallbackId = "tz_view"

pattern ReportEntrypointCallbackId :: Text
pattern ReportEntrypointCallbackId = "tz_report"

---------------------------
-- block action identifiers
---------------------------

-- message view modal, "report" button
reportButtonActionId :: ActionId
reportButtonActionId = "report-button-action-id"

reportButtonBlockId :: BlockId
reportButtonBlockId = "report-button-block-id"

type ReportInputElementActionId = "report-input-element-action-id"
type ReportInputBlockId = "report-input-block-id"

-- report view, plain text input block
reportInputElementActionId :: ActionId
reportInputElementActionId = fromString $ symbolVal $ Proxy @ReportInputElementActionId

reportInputBlockId :: BlockId
reportInputBlockId = fromString $ symbolVal $ Proxy @ReportInputBlockId

---------------------------
-- modal types
---------------------------
viewModal :: Text
viewModal = "view_modal"

reportModal :: Text
reportModal = "report_modal"

---------------------------
-- getting help
---------------------------

pattern HelpCommand :: Text
pattern HelpCommand = "/tzhelp"

helpUsage :: Text
helpUsage = [int||_Type #{HelpCommand} to find out how the tzbot works._|]

helpMessage :: Text
helpMessage = [int|n|
Hello!

I am *tzbot* - the time zone bot.

I can capture time references inside messages and translate them to your
time zone.
Whenever the message is posted in the channel I'm in, if this
message contains any time references, I will translate them and send it
to you via an ephemeral message. If a time reference is invalid for some
reason, I will say what's wrong with it.

Also you can trigger the message translation yourself,
by using entrypoints in the Slack message context menu; this works
in every channel of the workspace (including direct messages).
There you also can report the bot working wrongly,
we will be very grateful for your feedback!
|]
