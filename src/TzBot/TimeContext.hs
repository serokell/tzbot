
-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.TimeContext where

import Universum hiding (many, toList, try)

import Control.Lens.TH (makeLensesWith)
import TzBot.Instances ()
import TzBot.TimeReference
import TzBot.Util

data TimeContext = TimeContext
  { tcCurrentDateRef :: Maybe DateReference
  , tcCurrentLocRef :: Maybe LocationReference
  } deriving stock (Show, Eq, Generic)

emptyTimeContext :: TimeContext
emptyTimeContext = TimeContext Nothing Nothing

makeLensesWith postfixFields ''TimeContext
