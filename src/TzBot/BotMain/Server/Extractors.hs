-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

{- | This module contains extractors that are similar to ones defined in the `slacker`
 - package, but adapted to server incoming requests.
 -}
module TzBot.BotMain.Server.Extractors where

import Universum

import Data.Aeson (Value)
import Data.Aeson.Lens (AsPrimitive(_String), AsValue(_Array), key)
import Slacker.SocketMode (EventWrapper(..))

getEvent :: Value -> Maybe (Text, Value)
getEvent evt =
  (,) <$> evt ^? key "type" . _String
      <*> pure evt

pattern EventValueServer :: Text -> Value -> EventWrapper
pattern EventValueServer typ event <-
        EventWrapper
          { ewEvent = getEvent -> Just (typ, event)
          , ewType = "event_callback"
          }

pattern BlockActionServer :: Text -> Value -> Value
pattern BlockActionServer actionId val <-
  (getEvent -> Just ("block_actions", getAction -> Just (actionId, val)))

getAction :: Value -> Maybe (Text, Value)
getAction evt = do
  [action] <- toList <$> evt ^? key "actions" . _Array
  (,) <$> (action ^? key "action_id" . _String) <*> pure evt

pattern InteractiveServer :: Text -> Value -> Value
pattern InteractiveServer typ val <- (getEvent -> Just (typ, val))
