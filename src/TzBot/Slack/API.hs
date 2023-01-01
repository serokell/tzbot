-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Slack.API
  ( API
  , api
  , SlackResponse(..)
  , Message(..)
  , parseMessage
  , User(..)
  , UserId(..)
  , ChannelId(..)
  , ThreadId(..)
  , MessageId(..)
  , Limit(..)
  , PostEphemeralReq(..)
  , PostMessageReq(..)
  , OpenViewReq(..)
  , UpdateViewReq(..)
  , TriggerId(..)
  , ViewId(..)
  , Modal(..)
  , module TzBot.Slack.API.Block
  , module TzBot.Slack.API.Common
  ) where

import Universum

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Time.Zones.All (TZLabel)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Servant
  (Get, JSON, Post, QueryParam, QueryParam', ReqBody, Required, Strict, ToHttpApiData, type (:<|>),
  type (:>))
import Servant.Auth (Auth, JWT)

import Data.Aeson.Types
import Data.List.NonEmpty qualified as NE
import Data.Time
import Formatting
import TzBot.Instances ()
import TzBot.Slack.API.Block
import TzBot.Slack.API.Common
import TzBot.Util

----------------------------------------------------------------------------
-- API
----------------------------------------------------------------------------

type RequiredParam = QueryParam' [Required, Strict]

-- | A type describing Slack's Web API: https://api.slack.com/web
type API =
  -- See: https://api.slack.com/methods/users.info
  Auth '[JWT] Text
    :> "users.info"
    :> RequiredParam "user" UserId
    :> Post '[JSON] (SlackResponse "user" User)
  :<|>
  -- See: https://api.slack.com/methods/conversations.members
  Auth '[JWT] Text
    :> "conversations.members"
    :> RequiredParam "channel" ChannelId
    :> RequiredParam "limit" Limit
    :> Get '[JSON] (SlackResponse "members" [UserId])
  :<|>
  -- See: https://api.slack.com/methods/chat.postEphemeral
  Auth '[JWT] Text
    :> "chat.postEphemeral"
    :> ReqBody '[JSON] PostEphemeralReq
    :> Post '[JSON] (SlackResponse "message_ts" Value)
  :<|>
  -- See: https://api.slack.com/methods/chat.postMessage
  Auth '[JWT] Text
    :> "chat.postMessage"
    :> ReqBody '[JSON] PostMessageReq
    :> Post '[JSON] (SlackResponse "ts" MessageId)
  :<|>
  -- See https://api.slack.com/methods/conversations.history
  Auth '[JWT] Text
    :> "conversations.history"
    :> RequiredParam "channel" ChannelId
    :> QueryParam "inclusive" Bool
    :> QueryParam "limit" Limit
    -- Maybe use timestamps here?
    :> QueryParam "oldest" MessageId
    :> QueryParam "latest" MessageId
    :> Post '[JSON] (SlackResponse "messages" [Message])
  :<|>
  -- See https://api.slack.com/methods/conversations.replies
  Auth '[JWT] Text
    :> "conversations.replies"
    :> RequiredParam "channel" ChannelId
    :> RequiredParam "ts" ThreadId
    :> QueryParam "inclusive" Bool
    :> QueryParam "limit" Limit
    -- Maybe use timestamps here?
    :> QueryParam "oldest" MessageId
    :> QueryParam "latest" MessageId
    :> Post '[JSON] (SlackResponse "messages" [Message])
  :<|>
  -- See https://api.slack.com/methods/views.open
  Auth '[JWT] Text
    :> "views.open"
    :> ReqBody '[JSON] OpenViewReq
    :> Post '[JSON] (SlackResponse "view" Value)
  :<|>
  -- See https://api.slack.com/methods/views.update
  Auth '[JWT] Text
    :> "views.update"
    :> ReqBody '[JSON] UpdateViewReq
    :> Post '[JSON] (SlackResponse "view" Value)

api :: Proxy API
api = Proxy

{- | Slack's responses are always wrapped in a json object with one of these two formats:

> {
>   "ok": true,
>   "some-key": { /* response */ }
> }

> {
>   "ok": false,
>   "error": "error_code"
> }

This data type helps abstract over that pattern.
-}
data SlackResponse (key :: Symbol) a
  = SRSuccess a
  | SRError Text (Maybe Value)
  deriving stock (Show)

instance (KnownSymbol key, FromJSON a) => FromJSON (SlackResponse key a) where
  parseJSON = withObject "SlackResponse" \obj -> do
    obj .: "ok" >>= \case
      True  -> SRSuccess <$> obj .: key
      False -> SRError <$> obj .: "error" <*> obj .:? "response_metadata"
    where
      key = Key.fromString $ symbolVal (Proxy @key)


----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

newtype UserId = UserId { unUserId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (ToHttpApiData, FromJSON, ToJSON)

newtype ChannelId = ChannelId { unChannelId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (ToHttpApiData, FromJSON, ToJSON)

newtype ThreadId = ThreadId { unThreadId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (ToHttpApiData, FromJSON, ToJSON, Buildable)

newtype MessageId = MessageId { unMessageId :: Text }
  deriving stock (Eq, Show, Ord)
  deriving newtype (ToHttpApiData, FromJSON, ToJSON, Buildable)

newtype Limit = Limit { limitQ :: Int}
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToHttpApiData)

newtype TriggerId = TriggerId { unTriggerId :: Text }
  deriving stock (Eq, Show, Ord)
  deriving newtype (ToJSON, FromJSON)

newtype ViewId = ViewId { unViewId :: Text }
  deriving stock (Eq, Show, Ord)
  deriving newtype (IsString, ToJSON, FromJSON)

-- See
data Message = Message
  { mUser :: UserId
  , mText :: Text
  , mMessageId :: MessageId
  , mTs :: UTCTime
  , mThreadId :: Maybe ThreadId
  , mEdited :: Bool
  , mSubType :: Maybe Text
  } deriving stock (Eq, Show, Generic)

instance FromJSON Message where
  parseJSON = withObject "Message" parseMessage

parseMessage :: Object -> Parser Message
parseMessage o = do
  mUser <- o .: "user"
  mText <- o .: "text"
  mMessageId <- o .: "ts"
  mTs <- fetchSlackTimestamp "ts" o
  mThreadId <- o .:? "thread_ts"
  mSubType <- o .:? "subtype"
  mEdited <- fmap (isJust @Text) . runMaybeT $
    MaybeT (o .:? "edited") >>= MaybeT . (.:? "ts")
  pure Message {..}



-- | See: https://api.slack.com/types/user
data User = User
  { uId :: UserId
  , uIsBot :: Bool
  , uTz :: TZLabel
  } deriving stock (Eq, Show, Generic)
    deriving FromJSON via RecordWrapper User

---------------------------------------------------------
-- | See https://api.slack.com/methods/chat.postEphemeral
data PostEphemeralReq = PostEphemeralReq
  { perUser :: UserId
  , perChannel :: ChannelId
  , perThreadTs :: Maybe ThreadId
  , perText :: Text
  -- ^ This field is used differently depending on whether
  -- blocks are present or absent, see the link.
  , perBlocks :: Maybe (NE.NonEmpty Block)
  } deriving stock (Eq, Show, Generic)
    deriving ToJSON via RecordWrapper PostEphemeralReq

-- | See https://api.slack.com/methods/chat.postMessage
data PostMessageReq = PostMessageReq
  { pmrChannel :: ChannelId
  , pmrText :: Text
  -- ^ This field is used differently depending on whether
  -- blocks are present or absent, see the link.
  , pmrBlocks :: Maybe (NE.NonEmpty Block)
  } deriving stock (Eq, Show, Generic)
    deriving ToJSON via RecordWrapper PostMessageReq

-- | See https://api.slack.com/methods/views.open
data OpenViewReq = OpenViewReq
  { ovrView :: Modal
  , ovrTriggerId :: TriggerId
  } deriving stock (Eq, Show, Generic)
    deriving ToJSON via RecordWrapper OpenViewReq

-- | See https://api.slack.com/methods/views.update
data UpdateViewReq = UpdateViewReq
  { uvrView :: Modal
  , uvrViewId :: ViewId
  } deriving stock (Eq, Show, Generic)
    deriving ToJSON via RecordWrapper UpdateViewReq

-- | See https://api.slack.com/reference/surfaces/views
data Modal = Modal
  { mTitle :: PlainText
  , mSubmit :: Maybe PlainText
  , mNotifyOnClose :: Bool
  , mCallbackId :: Text
  , mPrivateMetadata :: Text
  , mBlocks :: [Block]
  } deriving stock (Eq, Show, Generic)
    deriving ToJSON via TypedWrapper Modal

{-
>>> encode testModal
"{\"blocks\":[],\"submit\":{\"text\":\"Report\",\"type\":\"plain_text\"},\"title\":{\"text\":\"Title\",\"type\":\"plain_text\"},\"type\":\"modal\"}"
 -}
