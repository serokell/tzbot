-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvent (
  processEvent
  ) where

import Universum

import Control.Concurrent.Async.Lifted (forConcurrently_)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set qualified as S

import TzBot.Parser (parseTimeRefs)
import TzBot.Slack
import TzBot.Slack.API
import TzBot.Slack.EphemeralMessage
import TzBot.Slack.Events (Message(..), MessageDetails(..), MessageEvent(..))
import TzBot.TimeReference (TimeReference(trText), TimeReferenceText, timeReferenceToUTC)
import TzBot.Util (attach)

-- Helper function for `filterNewReferencesAndMemorize`
filterNewReferencesAndMemorizePure :: [TimeReference] -> S.Set TimeReferenceText -> (S.Set TimeReferenceText, [TimeReference])
filterNewReferencesAndMemorizePure newRefs refsSet = foldl' f (refsSet, []) newRefs
  where
    f (accSet, accRefs) ref = do
      let newAccSet = S.insert (trText ref) accSet
      if S.size newAccSet == S.size accSet
        then (newAccSet, accRefs)
        else (newAccSet, ref : accRefs)

-- | When an edited message comes, define what time references in it haven't appear before
-- and record them in the bot's state.
filterNewReferencesAndMemorize :: MessageId -> [TimeReference] -> BotM (Maybe (NE.NonEmpty TimeReference))
filterNewReferencesAndMemorize messageId timeRefs = NE.nonEmpty <$> do
  processedRefsIORef <- asks bsMessagesReferences
  atomicModifyIORef' processedRefsIORef $ \refsMap -> do
    flip runState timeRefs $ M.alterF f messageId refsMap
  where
    f :: Maybe (S.Set TimeReferenceText) -> State [TimeReference] (Maybe (S.Set TimeReferenceText))
    f mbSet = state $ \refs -> do
      case mbSet of
        Nothing  -> (Just $ S.fromList $ map trText refs, refs)
        Just set -> bimap Just reverse $ filterNewReferencesAndMemorizePure refs set

-- We don't need to handle MDMessageBroadcast anyhow,
-- because we were supposed to reply to the original message
-- in the thread earlier.
newMessageOrEditedMessage :: MessageEvent -> Bool
newMessageOrEditedMessage evt = case meMessageDetails evt of
  MDMessage          -> True
  MDMessageEdited {} -> True
  MDMessageBroadcast -> False

-- | Process incoming Slack events. Only "message" and "message_edited" are processed.
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
processEvent :: MessageEvent -> BotM ()
processEvent evt = when (newMessageOrEditedMessage evt) $ do
  let message = meMessage evt

  -- TODO: use some "transactions here"? Or just lookup the map multiple times.
  mbReferencesToCheck <- filterNewReferencesAndMemorize (mMessageId message) (parseTimeRefs $ mText message)
  whenJust mbReferencesToCheck $ \timeRefs-> do
    sender <- getUser $ mUser message
    let now = meTs evt
        timeRefsUtc = attach (timeReferenceToUTC (uTz sender) now) timeRefs
        channelId = meChannel evt
    let sendAction = sendEphemeralMessage channelId (mThreadId message)

    -- TODO: We definitely need some caching here
    usersInChannelIds <- getChannelMembers channelId
    let ephemeralTemplate = NE.map (renderEphemeralMessageTemplate now sender) timeRefsUtc

    whenJust (renderErrorsForSender ephemeralTemplate) $ \errorsMsg ->
      sendAction errorsMsg (uId sender)

    let notBotAndSameTimeZone u = not (uIsBot u) && uTz u /= uTz sender
        notSender userId = userId /= uId sender
    usersToNotify <- filter notBotAndSameTimeZone <$> (mapM getUser $ filter notSender usersInChannelIds)
    -- usersToNotify <- mapM getUser usersInChannelIds -- dev

    forConcurrently_ usersToNotify $ \userToNotify -> do
      let ephemeralMessage = renderEphemeralMessageForOthers userToNotify ephemeralTemplate
      sendAction ephemeralMessage (uId userToNotify)
