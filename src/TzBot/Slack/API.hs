-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Slack.API
  ( API
  , api
  , SlackResponse(..)
  , SlackContents(..)
  , Message(..)
  , parseMessage
  , User(..)
  , UserId(..)
  , ChannelId(..)
  , ChannelType(..)
  , Cursor(..)
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

import Data.Aeson.Key qualified as Key
import Data.Aeson.Types
import Data.List.NonEmpty qualified as NE
import Data.Time (UTCTime)
import Data.Time.Zones.All (TZLabel)
import Formatting (Buildable)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Servant
  (Get, JSON, Post, QueryParam, QueryParam', ReqBody, Required, Strict, ToHttpApiData, type (:<|>),
  type (:>))
import Servant.Auth (Auth, JWT)
import Text.Interpolation.Nyan (int, rmode')

import TzBot.Instances ()
import TzBot.Slack.API.Block
import TzBot.Slack.API.Common
import TzBot.Slack.API.MessageBlock (MessageBlock)
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
    :> Post '[JSON] (SlackResponse $ SlackContents "user" User)
  :<|>
  -- See: https://api.slack.com/methods/conversations.members
  Auth '[JWT] Text
    :> "conversations.members"
    :> RequiredParam "channel" ChannelId
    :> RequiredParam "limit" Limit
    :> QueryParam "cursor" Cursor
    :> Get '[JSON] (SlackResponse $ SlackContents "members" $ [UserId])
  :<|>
  -- See: https://api.slack.com/methods/chat.postEphemeral
  Auth '[JWT] Text
    :> "chat.postEphemeral"
    :> ReqBody '[JSON] PostEphemeralReq
    :> Post '[JSON] (SlackResponse $ SlackContents "message_ts" Value)
  :<|>
  -- See: https://api.slack.com/methods/chat.postMessage
  Auth '[JWT] Text
    :> "chat.postMessage"
    :> ReqBody '[JSON] PostMessageReq
    :> Post '[JSON] (SlackResponse $ SlackContents "ts" MessageId)
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
    :> Post '[JSON] (SlackResponse $ SlackContents "messages" [Message])
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
    :> Post '[JSON] (SlackResponse $ SlackContents "messages" [Message])
  :<|>
  -- See https://api.slack.com/methods/views.open
  Auth '[JWT] Text
    :> "views.open"
    :> ReqBody '[JSON] OpenViewReq
    :> Post '[JSON] (SlackResponse $ SlackContents "view" Value)
  :<|>
  -- See https://api.slack.com/methods/views.update
  Auth '[JWT] Text
    :> "views.update"
    :> ReqBody '[JSON] UpdateViewReq
    :> Post '[JSON] (SlackResponse $ SlackContents "view" Value)

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
data SlackResponse a
  = SRSuccess a
  | SRError Text (Maybe Value)
  deriving stock (Show)

instance (FromJSON a) => FromJSON (SlackResponse a) where
  parseJSON val = ($ val) $ withObject "SlackResponse" \obj -> do
    obj .: "ok" >>= \case
      True  -> SRSuccess <$> parseJSON val
      False -> SRError <$> obj .: "error" <*> obj .:? "response_metadata"

data SlackContents (key :: Symbol) a = SlackContents
  { scContents :: a
  , scCursor :: Maybe Cursor
  } deriving stock (Show)

instance (KnownSymbol key, FromJSON a) => FromJSON (SlackContents key a) where
  parseJSON = withObject "SlackContents" \obj -> do
    scCursor <- runMaybeT $ do
      cursor <- MaybeT (obj .:? "response_metadata") >>= MaybeT . (.:? "next_cursor")
      guard $ not $ null $ unCursor cursor
      pure cursor
    let key = Key.fromString $ symbolVal (Proxy @key)
    scContents <- obj .: key
    pure SlackContents {..}

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

newtype UserId = UserId { unUserId :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToHttpApiData, FromJSON, ToJSON, Hashable, Buildable, IsString)

newtype ChannelId = ChannelId { unChannelId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (ToHttpApiData, FromJSON, ToJSON, Hashable, Buildable)

data ChannelType
  = CTChannel       -- ^ Public channel
  | CTGroup         -- ^ Private channel
  | CTDirectChannel -- ^ Direct messages
  deriving stock (Eq, Show)

instance FromJSON ChannelType where
  parseJSON = withText "ChannelType" $ \case
    "channel" -> pure CTChannel
    "group"   -> pure CTGroup
    "im"      -> pure CTDirectChannel
    unknown   -> fail [int||unknown channel type #{unknown}|]

newtype ThreadId = ThreadId { unThreadId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (ToHttpApiData, FromJSON, ToJSON, Buildable)

newtype MessageId = MessageId { unMessageId :: Text }
  deriving stock (Eq, Show, Ord)
  deriving newtype (ToHttpApiData, FromJSON, ToJSON, Buildable)

newtype Limit = Limit { limitQ :: Int}
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToHttpApiData)

newtype Cursor = Cursor { unCursor :: Text }
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToHttpApiData)

newtype TriggerId = TriggerId { unTriggerId :: Text }
  deriving stock (Eq, Show, Ord)
  deriving newtype (ToJSON, FromJSON)

newtype ViewId = ViewId { unViewId :: Text }
  deriving stock (Eq, Show, Ord)
  deriving newtype (IsString, ToJSON, FromJSON, Buildable)

-- See https://api.slack.com/events/message
data Message = Message
  { mUser :: UserId
  , mText :: Text
  , mMessageId :: MessageId
  , mTs :: UTCTime
  , mThreadId :: Maybe ThreadId
  , mEdited :: Bool
  , mSubType :: Maybe Text
  , msgBlocks :: WithUnknown [MessageBlock]
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
  msgBlocks <- o .: "blocks"
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
