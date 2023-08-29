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

import TzPrelude

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

Invite me to a channel and whenever a message with time references such as
"Let's meet at 10am" is posted, I will convert them to your time zone.
If a time reference is invalid for some reason, I will tell you why and how
to fix it.

You can also manually ask me to translate time references by opening the
context menu `⋮` next to a message and selecting "Translate time references".
This works in all channels and direct messages.

Finally, you can select "Report an issue" from the context menu `⋮` to
report any issue with the bot. Your feedback is greatly appreciated!
|]
