-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvents.Message
  ( processMessageEvent
  ) where

import Universum

import Control.Concurrent.Async.Lifted (forConcurrently_)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set qualified as S

import TzBot.Parser (parseTimeRefs)
import TzBot.Render
import TzBot.Slack
import TzBot.Slack.API
import TzBot.Slack.Events
import TzBot.Slack.Fixtures qualified as Fixtures
import TzBot.TimeReference (TimeReference(trText), TimeReferenceText)

-- Helper function for `filterNewReferencesAndMemorize`
filterNewReferencesAndMemorizePure
  :: [TimeReference]
  -> S.Set TimeReferenceText
  -> (S.Set TimeReferenceText, [TimeReference])
filterNewReferencesAndMemorizePure newRefs refsSet =
  foldl' f (refsSet, []) newRefs
  where
    f (accSet, accRefs) ref = do
      let newAccSet = S.insert (trText ref) accSet
      if S.size newAccSet == S.size accSet
        then (newAccSet, accRefs)
        else (newAccSet, ref : accRefs)

-- | When an edited message comes, define what time references in it
-- haven't appear before and record them in the bot's state.
filterNewReferencesAndMemorize
  :: MessageId
  -> [TimeReference]
  -> BotM (Maybe (NE.NonEmpty TimeReference))
filterNewReferencesAndMemorize messageId timeRefs = NE.nonEmpty <$> do
  processedRefsIORef <- asks bsMessagesReferences
  atomicModifyIORef' processedRefsIORef $ \refsMap -> do
    flip runState timeRefs $ M.alterF f messageId refsMap
  where
    f :: Maybe (S.Set TimeReferenceText)
      -> State [TimeReference] (Maybe (S.Set TimeReferenceText))
    f mbSet = state $ \refs -> do
      case mbSet of
        Nothing  -> (Just $ S.fromList $ map trText refs, refs)
        Just set -> bimap Just reverse $ filterNewReferencesAndMemorizePure refs set

-- We don't need to handle MDMessageBroadcast anyhow,
-- because we were supposed to reply to the original message
-- in the thread earlier.
newMessageOrEditedMessage :: MessageEvent -> Bool
newMessageOrEditedMessage evt = case meMessageDetails evt of
  MDMessage           -> True
  MDMessageEdited {}  -> True
  MDMessageBroadcast  -> False
  MDUserJoinedChannel -> False
  MDUserLeftChannel   -> False

-- | Process incoming Slack message events. Only "message" and "message_edited" are processed.
--
-- For "message" event from the conversation, if the message contains any time references,
-- get all users of that conversation, then send ephemerals to them,
-- containing translation of all found time references; send an ephemeral to the sender
-- if the original message contains some possibly invalid/ambiguous references or unknown
-- offset abbreviations. For messages in a thread, send ephemerals to that thread
-- (regardless of possible thread broadcast).
--
-- "message_edited" event is processed similarly, but only most recently added time
-- references are processed and sent via ephemerals.
processMessageEvent :: MessageEvent -> BotM ()
processMessageEvent evt = when (newMessageOrEditedMessage evt) $ do
  let msg = meMessage evt

  -- TODO: use some "transactions here"? Or just lookup the map multiple times.
  mbReferencesToCheck <-
    filterNewReferencesAndMemorize (mMessageId msg) (parseTimeRefs $ mText msg)
  whenJust mbReferencesToCheck $ \timeRefs-> do
    sender <- getUserCached $ mUser msg
    let now = meTs evt
        channelId = meChannel evt
    let sendAction :: Bool -> TranslationPairs -> UserId -> BotM ()
        sendAction toSender transl userId = do
          let req = PostEphemeralReq
                { perUser = userId
                , perChannel = channelId
                , perThreadTs = mThreadId msg
                , perText = joinTranslationPairs toSender transl
                , perBlocks = NE.nonEmpty $
                  renderSlackBlocks toSender (Just transl) <>
                  [ BSection $ textSection (PlainText "Something wrong?") Nothing
                  , BActions Actions
                    { aBlockId = Fixtures.reportFromEphemeralBlockId
                    , aElements = L.singleton $ Button
                      { bText = "Report an issue here"
                      , bActionId =
                          Fixtures.reportFromEphemeralActionId
                            (mMessageId msg)
                            (mThreadId msg)
                      }
                    }
                  ]
                }
          sendEphemeralMessage req

    usersInChannelIds <- getChannelMembersCached channelId
    let ephemeralTemplate = renderTemplate now sender timeRefs

    whenJust (renderErrorsForSenderTP ephemeralTemplate) $ \errorsMsg ->
      sendAction True errorsMsg (uId sender)

    let notBotAndSameTimeZone u = not (uIsBot u) && uTz u /= uTz sender
        notSender userId = userId /= uId sender

    let devEnv = False
    forConcurrently_ usersInChannelIds $ \userInChannelId ->
      if devEnv
      then do
        userInChannel <- getUserCached userInChannelId
        let ephemeralMessage = renderAllForOthersTP userInChannel ephemeralTemplate
        sendAction False ephemeralMessage (uId userInChannel)
      else
        when (notSender userInChannelId) $ do
          userInChannel <- getUserCached userInChannelId
          when (notBotAndSameTimeZone userInChannel) $ do
            let ephemeralMessage = renderAllForOthersTP userInChannel ephemeralTemplate
            sendAction False ephemeralMessage (uId userInChannel)
