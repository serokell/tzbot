-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvents.Common where

import Universum

import Data.GUID

import TzBot.Feedback.Dialog
import TzBot.Feedback.Dialog.Types
import TzBot.Parser (parseTimeRefs)
import TzBot.Render
import TzBot.Slack
import TzBot.Slack.API

openModalCommon
  :: Message
  -> UserId
  -> TriggerId
  -> (Text -> Maybe TranslationPairs -> ReportDialogId -> Modal)
  -> BotM ()
openModalCommon message whoTriggeredId triggerId mkModalFunc = do
  let msgText = mText message
      msgTimestamp = mTs message
  let mbTimeRefs = nonEmpty $ parseTimeRefs $ mText message
  sender <- getUser $ mUser message
  translationPairs <- forM mbTimeRefs $ \neTimeRefs -> do
      whoTriggered <- getUser whoTriggeredId
      pure $
        renderAllTP whoTriggered $ renderTemplate msgTimestamp sender neTimeRefs

  guid <- ReportDialogId <$> liftIO genText
  let metadata = ReportDialogEntry
        { rpmMessageText = mText message
        , rpmTimeTranslation = translationPairs
        , rpmSenderTimeZone = uTz sender
        , rpmMessageTimestamp = mTs message
        }
  insertDialogEntry guid metadata
  let modal = mkModalFunc msgText translationPairs guid
  startModal $ OpenViewReq modal triggerId
