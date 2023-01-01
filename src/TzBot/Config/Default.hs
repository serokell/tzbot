-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Config.Default (
  defaultConfigTrick,
  defaultConfigText,
  ) where

import Universum

import Text.Interpolation.Nyan

import TzBot.Config.Types
import TzBot.Instances ()
import TzBot.Util

defaultConfigText :: ByteString
defaultConfigText = [int||
# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

# Some of these settings can be provided by environment variables (envvars),
# which have highest priority.

# Slack application token,
# see https://api.slack.com/apps/[application_id] for details.
# Envvar: #{appTokenEnv}
#
# appToken: <app-token>

# Bot authentication token,
# see https://api.slack.com/apps/[application_id]/oauth for details.
# Envvar: #{botTokenEnv}
#
# botToken: <bot-token>

# Maximum allowed times to retry on 429 error before giving up.
# Envvar: #{maxRetriesEnv}
#
maxRetries: 3

# Caching expiration time for user profile info.
# Format examples: 2d18h40m, 3m20s, 20m.
# Envvar: #{cacheUsersEnv}
#
cacheUsersInfo: 3m

# Caching expiration time for channel members info.
# Format examples: 2d18h40m, 3m20s, 20m.
# Envvar: #{cacheConvMembersEnv}
#
cacheConversationMembers: 3m

# Defines what Slack channel will be used for collecting user feedback.
# Envvar: #{feedbackChannelEnv}
#
# feedbackChannel: C13FQHWLQS2

# Defines what file will be used for collecting user feedback.
# Envvar: #{feedbackFileEnv}
#
feedbackFile: feedback.log
  |]

-- This prevents Config.Default.defaultConfigText to be incorrect on compiling.
defaultConfigTrick :: Trick (Config 'CSInterm)
defaultConfigTrick = thTrickYaml defaultConfigText
