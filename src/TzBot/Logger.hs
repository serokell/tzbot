-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Logger
  ( module TzBot.Logger
  , KatipContext (..)
  , KatipContextT
  , katipAddNamespace
  , katipAddContext
  ) where

import TzPrelude

import Data.Aeson (KeyValue((.=)), ToJSON(..), object)
import Katip
import Text.Interpolation.Nyan (int, rmode's)

import TzBot.Slack.API (ChannelId(..), MessageId(..))

logSugar_ :: (KatipContext m, HasCallStack) => Severity -> Text -> m ()
logSugar_ sev = logLocM sev . logStr

logInfo, logWarn, logDebug, logError :: (KatipContext m, HasCallStack) => Text -> m ()
logInfo t = withFrozenCallStack $ logSugar_ InfoS t
logWarn t = withFrozenCallStack $ logSugar_ WarningS t
logDebug t = withFrozenCallStack $ logSugar_ DebugS t
logError t = withFrozenCallStack $ logSugar_ ErrorS t

logException :: (Exception e, KatipContext m) => e -> m ()
logException err = logError [int||Error occured: #s{err}|]

withLogger
  :: Severity
  -> ((Namespace, LogContexts, LogEnv) -> IO a)
  -> IO a
withLogger logLevel action = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem logLevel) V2
  let namespace = "main"
      initialContext = mempty
      environment = "general"
      mainNamespace = "TzBot"
      scribeName = "stdout"
      mkLogEnv =
        initLogEnv mainNamespace environment >>=
        registerScribe scribeName handleScribe defaultScribeSettings
  bracket mkLogEnv closeScribes $ \logEnv ->
    action (namespace, initialContext, logEnv)

katipAddNamespaceText :: (KatipContext m) => Text -> m a -> m a
katipAddNamespaceText txt = katipAddNamespace $ Namespace [txt]

----------------------------------------------------------------------------
-- Contexts
----------------------------------------------------------------------------

type EventType = Text
type EventId = Text
data EventContext = EventContext EventType EventId

instance ToJSON EventContext where
  toJSON (EventContext t cId) =
    object
      [ "event_type" .= t
      , "event_id" .= cId
      ]

instance ToObject EventContext

instance LogItem EventContext where
  payloadKeys _verb _a = AllKeys

-- | A message is uniquely identified by the Channel ID
-- and the message timestamp (i.e. `MessageId`).
data MessageContext = MessageContext ChannelId MessageId

instance ToJSON MessageContext where
  toJSON (MessageContext channelId msgId) =
    object
      [ "channel_id" .= channelId
      , "message_id" .= msgId
      ]

instance ToObject MessageContext

instance LogItem MessageContext where
  payloadKeys _verb _a = AllKeys
