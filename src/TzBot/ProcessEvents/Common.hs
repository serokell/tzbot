-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvents.Common
  ( openModalCommon
  ) where

import Universum

import Data.GUID (genText)

import TzBot.Feedback.Dialog (insertDialogEntry)
import TzBot.Feedback.Dialog.Types
import TzBot.Parser (parseTimeRefs)
import TzBot.Render (TranslationPairs, renderAllForOthersTP, renderTemplate)
import TzBot.Slack (BotM, getUserCached, startModal)
import TzBot.Slack.API

-- | Generic function that starts view or report modal where
--   time references translations can be viewed
--   and feedback can be left.
openModalCommon
  :: Message
  -> ChannelId
  -> UserId
  -> TriggerId
  -> (Text -> Maybe TranslationPairs -> ReportDialogId -> Modal)
  -- ^ The way how to build a modal.
  -- See "TzBot.Slack.Modal" for possible implementations
  -> BotM ()
openModalCommon message channelId whoTriggeredId triggerId mkModalFunc = do
  let msgText = mText message
      msgTimestamp = mTs message
  let mbTimeRefs = nonEmpty $ parseTimeRefs $ mText message
  sender <- getUserCached $ mUser message
  translationPairs <- forM mbTimeRefs $ \neTimeRefs -> do
      whoTriggered <- getUserCached whoTriggeredId
      pure $
        renderAllForOthersTP whoTriggered $
          renderTemplate msgTimestamp sender neTimeRefs

  guid <- ReportDialogId <$> liftIO genText
  let metadata = ReportDialogEntry
        { rpmMessageText = mText message
        , rpmTimeTranslation = translationPairs
        , rpmSenderTimeZone = uTz sender
        , rpmMessageTimestamp = mTs message
        , rpmUserId = whoTriggeredId
        , rpmChannelId = channelId
        , rpmThreadId = mThreadId message
        }
  insertDialogEntry guid metadata
  let modal = mkModalFunc msgText translationPairs guid
  startModal $ OpenViewReq modal triggerId
