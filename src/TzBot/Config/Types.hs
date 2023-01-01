-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE TypeFamilyDependencies #-}

module TzBot.Config.Types where

import Universum

import Data.Aeson
import Data.Data
import Time

import TzBot.Instances ()
import TzBot.Util

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

appTokenEnv, botTokenEnv, maxRetriesEnv, cacheUsersEnv, cacheConvMembersEnv :: EnvVarName
appTokenEnv = "SLACK_TZ_APP_TOKEN"
botTokenEnv = "SLACK_TZ_BOT_TOKEN"
maxRetriesEnv = "SLACK_TZ_MAX_RETRIES"
cacheUsersEnv = "SLACK_TZ_CACHE_USERS_INFO"
cacheConvMembersEnv = "SLACK_TZ_CACHE_CONVERSATION_MEMBERS"

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
  } deriving stock (Generic)

deriving stock instance Eq (Config 'CSInterm)
deriving stock instance Show (Config 'CSInterm)
instance FromJSON (Config 'CSInterm) where
  parseJSON = genericParseJSON aesonConfigOption
instance ToJSON (Config 'CSInterm) where
  toJSON = genericToJSON aesonConfigOption

deriving stock instance Eq (Config 'CSFinal)
deriving stock instance Show (Config 'CSFinal)
instance FromJSON (Config 'CSFinal) where
  parseJSON = genericParseJSON aesonConfigOption
