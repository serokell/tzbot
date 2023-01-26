-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE TypeFamilyDependencies #-}

module TzBot.Config.Types where

import Universum

import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), genericParseJSON, genericToJSON)
import Data.Data (Data)
import Time (Minute, Time)

import TzBot.Instances ()
import TzBot.Slack.API (ChannelId)
import TzBot.Util (aesonConfigOptions)

type FieldName = String
type EnvVarName = String
type Env = Map String String

newtype AppLevelToken = AppLevelToken
  { unAppLevelToken :: Text }
  deriving stock (Eq, Show, Generic, Data)
  deriving newtype (FromJSON, ToJSON, NFData)

newtype BotToken = BotToken
  { unBotToken :: Text }
  deriving stock (Eq, Show, Generic, Data)
  deriving newtype (FromJSON, ToJSON, NFData)

data ConfigStage =
    CSInterm -- ^ Some fields may be optional.
  | CSFinal  -- ^ All fields are present.

type family ConfigField (k :: ConfigStage) a where
  ConfigField 'CSInterm a = Maybe a
  ConfigField 'CSFinal a = a

type BotConfig = Config 'CSFinal

appTokenEnv, botTokenEnv, maxRetriesEnv,
  cacheUsersEnv, cacheConvMembersEnv,
  feedbackChannelEnv, feedbackFileEnv,
  cacheReportDialogEnv, inverseHelpUsageChanceEnv :: EnvVarName
appTokenEnv = "SLACK_TZ_APP_TOKEN"
botTokenEnv = "SLACK_TZ_BOT_TOKEN"
maxRetriesEnv = "SLACK_TZ_MAX_RETRIES"
cacheUsersEnv = "SLACK_TZ_CACHE_USERS_INFO"
cacheConvMembersEnv = "SLACK_TZ_CACHE_CONVERSATION_MEMBERS"
feedbackChannelEnv = "SLACK_TZ_FEEDBACK_CHANNEL"
feedbackFileEnv = "SLACK_TZ_FEEDBACK_FILE"
cacheReportDialogEnv = "SLACK_TZ_CACHE_REPORT_DIALOG"
inverseHelpUsageChanceEnv = "SLACK_TZ_INVERSE_HELP_USAGE_CHANCE"

-- | Overall config.
data Config f = Config
  { cAppToken                 :: ConfigField f AppLevelToken
    -- ^ Slack application token. See https://api.slack.com/apps/[application_id] for details.
  , cBotToken                 :: ConfigField f BotToken
    -- ^ Bot authentication token. See https://api.slack.com/apps/[application_id]/oauth for details.
  , cMaxRetries               :: Int
    -- ^ Maximum allowed times to retry on 429 error before giving up.
  , cCacheUsersInfo           :: Time Minute
    -- ^ Caching expiration time for user profile info.
  , cCacheConversationMembers :: Time Minute
    -- ^ Caching expiration time for channel members info.
  , cFeedbackChannel          :: Maybe ChannelId
    -- ^ Slack channel to send collected user feedback.
  , cFeedbackFile             :: Maybe FilePath
    -- ^ File path to record collected user feedback.
  , cCacheReportDialog        :: Time Minute
    -- ^ How long a report dialog is valid after its opening.
  , cInverseHelpUsageChance   :: Int
    -- ^ 1/p, where p is chance to append help command usage
    -- to the ephemeral message.
  } deriving stock (Generic)

deriving stock instance Eq (Config 'CSInterm)
deriving stock instance Show (Config 'CSInterm)
instance FromJSON (Config 'CSInterm) where
  parseJSON = genericParseJSON aesonConfigOptions
instance ToJSON (Config 'CSInterm) where
  toJSON = genericToJSON aesonConfigOptions

deriving stock instance Eq (Config 'CSFinal)
deriving stock instance Show (Config 'CSFinal)
instance FromJSON (Config 'CSFinal) where
  parseJSON = genericParseJSON aesonConfigOptions
