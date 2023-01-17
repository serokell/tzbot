-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Logger
  ( module TzBot.Logger
  , module Katip
  ) where

import Universum

import Data.Aeson (KeyValue((.=)), ToJSON(..), object)
import Katip

import TzBot.Slack.API (MessageId(..))

logSugar :: Severity -> (Severity -> LogStr -> m ()) -> Text -> m ()
logSugar sev logFunc = logFunc sev . logStr

debug, info, warn, error_ :: (Severity -> LogStr -> m ()) -> Text -> m ()
debug = logSugar DebugS
info = logSugar InfoS
warn = logSugar WarningS
error_ = logSugar ErrorS

infix 5 `debug`
infix 5 `info`
infix 5 `warn`
infix 5 `error_`

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

-- contexts
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

--
newtype MessageContext = MessageContext MessageId

instance ToJSON MessageContext where
  toJSON (MessageContext msgId) = object ["message_id" .= msgId]

instance ToObject MessageContext

instance LogItem MessageContext where
  payloadKeys _verb _a = AllKeys
