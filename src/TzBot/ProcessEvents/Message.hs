-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvents.Message
  ( processMessageEvent
  ) where

import Universum hiding (try)

import Control.Concurrent.Async.Lifted (forConcurrently)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text.Lazy.Builder (Builder)
import System.Random (randomRIO)
import Text.Interpolation.Nyan (int, rmode', rmode's)

import TzBot.Config (Config(..))
import TzBot.Logger
import TzBot.ProcessEvents.Common (getTimeReferencesFromMessage)
import TzBot.Render
import TzBot.Slack
import TzBot.Slack.API
import TzBot.Slack.Events
import TzBot.Slack.Fixtures qualified as Fixtures
import TzBot.TimeReference (TimeReference(trText), TimeReferenceText)
import TzBot.Util (catchAllErrors, isDevEnvironment, withMaybe)

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
-- channel_join and channel_leave messages are also ignored because
-- we handle these events in another way.
filterMessageTypeWithLog :: (KatipContext m) => MessageEvent -> m Bool
filterMessageTypeWithLog evt = case meMessageDetails evt of
  MDMessage          -> do
    logInfo [int||Handling new message|]
    pure True
  MDMessageEdited {} -> do
    logInfo [int||Message was edited|]
    pure True
  MDMessageBroadcast -> do
    logInfo [int||Incoming message is thread broadcast, ignoring|]
    pure False
  MDUserJoinedChannel -> do
    logInfo [int||Incoming message subtype=channel_join, ignoring|]
    pure False
  MDUserLeftChannel -> do
    logInfo [int||Incoming message subtype=channel_leave, ignoring|]
    pure False

withSenderNotBot :: MessageEvent -> BotM (Maybe User)
withSenderNotBot evt = do
  sender <- getUserCached $ mUser $ meMessage evt
  if uIsBot sender
    then do
      logInfo [int||Sender is bot, ignoring|]
      pure Nothing
    else pure $ Just sender

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
processMessageEvent evt =
  katipAddNamespaceText "message" $
  katipAddContext (MessageContext (mMessageId $ meMessage evt)) $
  whenM (filterMessageTypeWithLog evt) $
    whenJustM (withSenderNotBot evt) $ \sender -> do
  let msg = meMessage evt

  -- TODO: use some "transactions here"? Or just lookup the map multiple times.
  timeRefs <- getTimeReferencesFromMessage msg
  mbReferencesToCheck <-
    filterNewReferencesAndMemorize (mMessageId msg) timeRefs
  withMaybe mbReferencesToCheck
    (logInfo "No time references for processing found")
      \timeRefs -> do
    inverseChance <- asks $ cInverseHelpUsageChance . bsConfig

    whetherToShowHelpCmd <- liftIO $ fmap (== 1) $ randomRIO (1, inverseChance)
    when whetherToShowHelpCmd $ logDebug "appending help command usage"

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
                  [ BSection $ markdownSection (Mrkdwn Fixtures.helpUsage) Nothing
                    | whetherToShowHelpCmd
                  ]
                }
          sendEphemeralMessage req
    let ephemeralTemplate = renderTemplate now sender timeRefs
    case meChannelType evt of
      -- According to
      -- https://forums.slackcommunity.com/s/question/0D53a00008vsItQCAU
      -- it's not possible to add the bot to any existing DMs, so if
      -- the channel type of the message event is DM, it can only be
      -- the user-bot conversation. This means that the user wants
      -- to translate some time references and we send the translation
      -- only to him, showing it in the way how other users would see
      -- it if it were sent to the common channel.
      Just CTDirectChannel -> do
        let ephemeralMessage = renderAllForOthersTP sender ephemeralTemplate
        logInfo [int||Received message from the DM, sending translation \
                              to the author|]
        sendAction False ephemeralMessage (uId sender)
      _ -> do
        usersInChannelIds <- getChannelMembersCached channelId

        whenJust (renderErrorsForSenderTP ephemeralTemplate) $ \errorsMsg -> do
          logInfo
            [int||Found invalid time references, \
                      sending an ephemeral with them to the message sender|]
          sendAction True errorsMsg (uId sender)

        let notBotAndSameTimeZone u = not (uIsBot u) && uTz u /= uTz sender
            notSender userId = userId /= uId sender
            setSize = S.size usersInChannelIds

        logInfo [int||#{setSize} users in the channel #{channelId}, sending ephemerals|]
        eithRes <- forConcurrently (toList usersInChannelIds) $ \userInChannelId -> catchAllErrors $
          if isDevEnvironment
          then do
            userInChannel <- getUserCached userInChannelId
            let ephemeralMessage = renderAllForOthersTP userInChannel ephemeralTemplate
            sendAction False ephemeralMessage (uId userInChannel)
            pure True
          else do
            let whenT :: (Monad m) => Bool -> m Bool -> m Bool
                whenT cond_ action_ = if cond_ then action_ else pure False
            whenT (notSender userInChannelId) $ do
              userInChannel <- getUserCached userInChannelId
              whenT (notBotAndSameTimeZone userInChannel) $ do
                let ephemeralMessage = renderAllForOthersTP userInChannel ephemeralTemplate
                sendAction False ephemeralMessage (uId userInChannel)
                pure True

        let failedMsg = "Ephemeral sending failed" :: Builder
            logAll :: Either SomeException BotException -> BotM ()
            logAll (Left se) = logError [int||#{failedMsg}, unknown error occured: #s{se}|]
            logAll (Right ke) = logError [int||#{failedMsg}, #{displayException ke}|]

            processResult
              :: (Int, Int)
              -> Either (Either SomeException BotException) Bool
              -> BotM (Int, Int)
            processResult (oks_, errs_) eithRes_ = case eithRes_ of
              Left err_ -> logAll err_ >> pure (oks_, errs_ + 1)
              Right ok_ -> let oks_' = if ok_ then oks_ + 1 else oks_ in pure (oks_', errs_)
        (oks, errs) <- foldM processResult (0, 0) eithRes
        logInfo [int||#{oks} ephemeral sent successfully|]
        logInfo [int||#{errs} ephemerals failed|]
