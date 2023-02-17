-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvents.Message
  ( processMessageEvent
  ) where

import Universum hiding (try)

import Data.List (singleton)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as S
import Data.Text.Lazy.Builder (Builder)
import System.Random (randomRIO)
import Text.Interpolation.Nyan (int, rmode')
import UnliftIO qualified

import TzBot.Cache qualified as Cache
import TzBot.Config (Config(..))
import TzBot.Logger
import TzBot.ProcessEvents.Common (getTimeReferencesFromMessage)
import TzBot.Render
import TzBot.Slack
import TzBot.Slack.API
import TzBot.Slack.Events
import TzBot.Slack.Fixtures qualified as Fixtures
import TzBot.TimeReference (TimeReference)
import TzBot.Util (whenT, withMaybe)

data MessageEventType = METMessage | METMessageEdited
  deriving stock (Eq)

-- We don't need to handle MDMessageBroadcast anyhow,
-- because we were supposed to reply to the original message
-- in the thread earlier.
-- channel_join and channel_leave messages are also ignored because
-- we handle these events in another way.
filterMessageTypeWithLog :: (KatipContext m) => MessageEvent -> m (Maybe MessageEventType)
filterMessageTypeWithLog evt = case meMessageDetails evt of
  MDMessage          -> do
    logInfo [int||Handling new message|]
    pure $ Just METMessage
  MDMessageEdited {} -> do
    logInfo [int||Message was edited|]
    pure $ Just METMessageEdited
  MDMessageBroadcast -> do
    logInfo [int||Incoming message is thread broadcast, ignoring|]
    pure Nothing
  MDUserJoinedChannel -> do
    logInfo [int||Incoming message subtype=channel_join, ignoring|]
    pure Nothing
  MDUserLeftChannel -> do
    logInfo [int||Incoming message subtype=channel_leave, ignoring|]
    pure Nothing

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
  katipAddContext (MessageContext msgId) $
  whenJustM (filterMessageTypeWithLog evt) $ \mEventType ->
  whenJustM (withSenderNotBot evt) $ \sender -> do
    timeRefs <- getTimeReferencesFromMessage msg
    processMessageEvent' evt mEventType sender timeRefs
  where
  msg = meMessage evt
  msgId = mMessageId $ meMessage evt

processMessageEvent'
  :: MessageEvent
  -> MessageEventType
  -> User
  -> [TimeReference]
  -> BotM ()
processMessageEvent' evt mEventType sender timeRefs =
  case meChannelType evt of
    Just CTDirectChannel -> handleDirectMessage
    _ -> case mEventType of
      METMessageEdited -> handleMessageChanged
      METMessage -> handleNewMessage

  where

  msg = meMessage evt
  msgId = mMessageId msg
  channelId = meChannel evt
  now = meTs evt
  mbThreadId = mThreadId msg

  notBot u = not (uIsBot u)
  isSender userId = uId sender == userId

  getWhetherToShowHelpCmd :: BotM Bool
  getWhetherToShowHelpCmd = do
    inverseChance <- asks $ cInverseHelpUsageChance . bsConfig
    liftIO $ fmap (== 1) $ randomRIO (1, inverseChance)

  logNoTimeRefsFound :: KatipContext m => m ()
  logNoTimeRefsFound = logInfo "No time references for processing found"

  withNonEmptyTimeRefs
    :: (KatipContext m)
    => [TimeReference]
    -> (NonEmpty TimeReference -> m ())
    -> m ()
  withNonEmptyTimeRefs trs action =
    maybe logNoTimeRefsFound action (nonEmpty trs)

  sendAction
    :: Maybe Text
    -> SenderFlag
    -> TranslationPairs
    -> UserId
    -> BotM ()
  sendAction mbPermalinkForEdit toSender transl userId = do
    whetherToShowHelpCmd <- getWhetherToShowHelpCmd
    let mbEditBlock =
          withMaybe mbPermalinkForEdit [] \permalink ->
            singleton $ BSection $
              markdownSection $ Mrkdwn [int||
                <#{permalink}|Message #{msgId}> has been edited:
                |]

    let req = PostEphemeralReq
          { perUser = userId
          , perChannel = channelId
          , perThreadTs = mbThreadId
          , perText = joinTranslationPairs toSender transl
          , perBlocks = NE.nonEmpty $ concat
            [ mbEditBlock
            , renderSlackBlocks toSender (Just transl)
            , [ BSection $ markdownSection (Mrkdwn Fixtures.helpUsage)
              | whetherToShowHelpCmd ]
            ]
          }
    sendEphemeralMessage req

  handleMessageChanged :: BotM ()
  handleMessageChanged = katipAddNamespaceText "edit" do
    messageRefsCache <- asks bsMessageCache
    mbMessageRefs <- Cache.lookup msgId messageRefsCache
    -- if not found or expired, just ignore this message
    -- it's too old or just didn't contain any time refs
    whenJust mbMessageRefs $ \oldRefs -> do
      let newRefsFound = not $ all (`elem` oldRefs) timeRefs
      -- no new references found, ignoring
      when newRefsFound $ withNonEmptyTimeRefs timeRefs \neTimeRefs -> do
        Cache.insert msgId timeRefs messageRefsCache
        permalink <- getMessagePermalinkCached channelId msgId
        handleChannelMessageCommon (Just permalink) neTimeRefs

  handleNewMessage :: BotM ()
  handleNewMessage = do
    withNonEmptyTimeRefs timeRefs $ \neTimeRefs -> do
      -- save message only if time references are present
      asks bsMessageCache >>= Cache.insert msgId timeRefs
      handleChannelMessageCommon Nothing neTimeRefs

  handleChannelMessageCommon :: Maybe Text -> NonEmpty TimeReference -> BotM ()
  handleChannelMessageCommon mbPermalink neTimeRefs = do
    let ephemeralTemplate = renderTemplate asForMessageM now sender neTimeRefs

    let sendActionLocal userInChannelId = do
          userInChannel <- getUserCached userInChannelId
          whenT (notBot userInChannel) do
            let mbEphemeralMessage =
                  renderAllTP userInChannel ephemeralTemplate
            let senderFlag =
                  if isSender $ uId userInChannel
                  then asForSenderS
                  else asForOthersS
            withMaybe mbEphemeralMessage (pure False) \eph -> do
              sendAction mbPermalink senderFlag eph (uId userInChannel)
              pure True
    ephemeralsMailing channelId sendActionLocal

  handleDirectMessage :: BotM ()
  handleDirectMessage =
    when (mEventType /= METMessageEdited) $
    withNonEmptyTimeRefs timeRefs $ \neTimeRefs -> do
    -- According to
    -- https://forums.slackcommunity.com/s/question/0D53a00008vsItQCAU
    -- it's not possible to add the bot to any existing DMs, so if
    -- the channel type of the message event is DM, it can only be
    -- the user-bot conversation. This means that the user wants
    -- to translate some time references and we send the translation
    -- only to him, showing it in the way how other users would see
    -- it if it were sent to the common channel.
      let mbEphemeralMessage = renderAllTP sender $
            renderTemplate asForModalM now sender neTimeRefs
      whenJust mbEphemeralMessage $ \eph -> do
        logInfo [int||
          Received message from the DM, sending translation to the author
          |]
        sendAction Nothing asForOthersS eph (uId sender)

ephemeralsMailing
  :: ChannelId
  -> (UserId -> BotM Bool)
  -- ^ This function should return `True` if message was sent successfully and `False`
  -- if message shouldn't be sent (e.g. for bots or to sender in some cases). This is
  -- then used for logging statistics
  -> BotM ()
ephemeralsMailing channelId sendAction = do
  usersInChannelIds <- getChannelMembersCached channelId
  let setSize = S.size usersInChannelIds
  logInfo [int||#{setSize} users in the channel #{channelId}, sending ephemerals|]
  eithRes <- UnliftIO.forConcurrently (toList usersInChannelIds) $ UnliftIO.trySyncOrAsync . sendAction
  let failedMsg = "Ephemeral sending failed" :: Builder
      logAll :: SomeException -> BotM ()
      logAll se = logError [int||#{failedMsg}, #{displayException se}|]

      processResult
        :: (Int, Int)
        -> Either SomeException Bool
        -> BotM (Int, Int)
      processResult (oks_, errs_) eithRes_ = case eithRes_ of
        Left err_ -> logAll err_ >> pure (oks_, errs_ + 1)
        Right ok_ -> let oks_' = if ok_ then oks_ + 1 else oks_ in pure (oks_', errs_)
  (oks, errs) <- foldM processResult (0, 0) eithRes
  logInfo [int||#{oks} ephemeral sent successfully|]
  logInfo [int||#{errs} ephemerals failed|]
