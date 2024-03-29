-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvents.Message
  ( processMessageEvent
  ) where

import TzPrelude hiding (try)

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

data MessageEventType
  = METMessage
  | METMessageEdited
    -- ^ A message has been edited.
      Message
      -- ^ The state of the message _before_ it was edited.
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
  MDMessageEdited previousMsg -> do
    logInfo [int||Message was edited|]
    pure $ Just $ METMessageEdited previousMsg
  MDMessageBroadcast -> do
    logInfo [int||Incoming message is thread broadcast, ignoring|]
    pure Nothing
  MDUserJoinedChannel -> do
    logInfo [int||Incoming message subtype=channel_join, ignoring|]
    pure Nothing
  MDUserLeftChannel -> do
    logInfo [int||Incoming message subtype=channel_leave, ignoring|]
    pure Nothing
  MDMessageUrlUnfurl -> do
    logInfo [int||Incoming message with URL preview, ignoring|]
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
-- containing conversions of all found time references; send an ephemeral to the sender
-- if the original message contains some possibly invalid/ambiguous references or unknown
-- offset abbreviations. For messages in a thread, send ephemerals to that thread
-- (regardless of possible thread broadcast).
--
-- "message_edited" event is processed similarly, but only most recently added time
-- references are processed and sent via ephemerals.
processMessageEvent :: MessageEvent -> BotM ()
processMessageEvent evt =
  katipAddNamespaceText "message" $
  katipAddContext (MessageContext evt.meChannel evt.meMessage.mMessageId evt.meMessage.mThreadId) $
  whenJustM (filterMessageTypeWithLog evt) $ \mEventType ->
  whenJustM (withSenderNotBot evt) $ \sender -> do
    timeRefs <- getTimeReferencesFromMessage evt.meMessage
    processMessageEvent' evt mEventType sender timeRefs

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
      METMessageEdited previousMsg -> handleMessageChanged previousMsg
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
    -> ConversionPairs
    -> UserId
    -> BotM ()
  sendAction mbPermalinkForEdit toSender conversionPairs userId = do
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
          , perText = joinConversionPairs toSender conversionPairs
          , perBlocks = NE.nonEmpty $ concat
            [ mbEditBlock
            , renderSlackBlocks toSender (Just conversionPairs)
            , [ BSection $ markdownSection (Mrkdwn Fixtures.helpUsage)
              | whetherToShowHelpCmd ]
            ]
          }
    sendEphemeralMessage req

  handleMessageChanged :: Message -> BotM ()
  handleMessageChanged previousMsg = katipAddNamespaceText "edit" do
    messageRefsCache <- asks bsMessageCache
    -- Fetch the time references from the old message (before it was edited).
    -- If they're not found in the cache, parse the message.
    oldRefs <- do
      Cache.lookup msgId messageRefsCache >>= \case
        Just cachedTimeRefs -> pure cachedTimeRefs
        Nothing -> getTimeReferencesFromMessage previousMsg
    let newRefsFound = not $ all (`elem` oldRefs) timeRefs
    -- If no new references are found, we ignore the edited message.
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
    let ephemeralTemplate = renderTemplate IsEphemeral now sender neTimeRefs

    let sendActionLocal userInChannelId = do
          userInChannel <- getUserCached userInChannelId
          whenT (notBot userInChannel) do
            let mbEphemeralMessage =
                  renderAllConversionPairs userInChannel ephemeralTemplate
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
    withNonEmptyTimeRefs timeRefs $ \neTimeRefs -> do
    -- According to
    -- https://forums.slackcommunity.com/s/question/0D53a00008vsItQCAU
    -- it's not possible to add the bot to any existing DMs, so if
    -- the channel type of the message event is DM, it can only be
    -- the user-bot conversation. This means that the user wants
    -- to convert some time references and we send the conversion
    -- only to them, showing it in the way how other users would see
    -- it if it were sent to the common channel.
      let mbEphemeralMessage = renderAllConversionPairs sender $
            renderTemplate IsModal now sender neTimeRefs
      whenJust mbEphemeralMessage $ \eph -> do
        logInfo [int||
          Received message from the DM, sending conversion to the author
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
