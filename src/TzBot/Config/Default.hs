-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Config.Default
  ( defaultConfigTrick
  , defaultConfigText
  ) where

import Universum

import Text.Interpolation.Nyan (int, rmode')

import TzBot.Config.Types qualified as CT
import TzBot.Instances ()
import TzBot.Util (Trick, thTrickYaml)

defaultConfigText :: ByteString
defaultConfigText = [int||
# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

# Some of these settings can be provided by environment variables (envvars),
# which have highest priority.

# Slack application token,
# see https://api.slack.com/apps/[application_id] for details.
# Envvar: #{CT.appTokenEnv}
#
# appToken: <app-token>

# Bot authentication token,
# see https://api.slack.com/apps/[application_id]/oauth for details.
# Envvar: #{CT.botTokenEnv}
#
# botToken: <bot-token>

# Maximum allowed times to retry on 429 error before giving up.
# Envvar: #{CT.maxRetriesEnv}
#
maxRetries: 3

# Caching expiration time for user profile info.
# Format examples: 2d18h40m, 3m20s, 20m.
# Envvar: #{CT.cacheUsersEnv}
#
cacheUsersInfo: 3m

# Caching expiration time for channel members info.
# Format examples: 2d18h40m, 3m20s, 20m.
# Envvar: #{CT.cacheConvMembersEnv}
#
cacheConversationMembers: 3m

# Defines what Slack channel will be used for collecting user feedback.
# Envvar: #{CT.feedbackChannelEnv}
#
# feedbackChannel: C13FQHWLQS2

# Defines what file will be used for collecting user feedback.
# Envvar: #{CT.feedbackFileEnv}
#
feedbackFile: feedback.log

# Defines how long a report dialog is valid after its opening.
# Envvar: #{CT.cacheReportDialogEnv}
#
cacheReportDialog: 1h

# Inverse chance of appending help command usage hint to the ephemeral
# message containing time translations.
# Envvar: #{CT.inverseHelpUsageChanceEnv}
#
inverseHelpUsageChance: 15

# Minimal permitted logging level.
# Available: Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency
# Envvar: #{CT.logLevelEnv}
#
logLevel: Info


# Port on which to run (server mode only).
# Envvar : #{CT.serverPortEnv}
#
port: 8912
  |]

-- This prevents Config.Default.defaultConfigText to be incorrect on compiling.
defaultConfigTrick :: Trick (CT.Config 'CT.CSInterm)
defaultConfigTrick = thTrickYaml defaultConfigText
