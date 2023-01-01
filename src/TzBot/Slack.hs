-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Slack
  ( BotM(..)
  , runBotM
  , runOrThrowBotM
  , AppLevelToken(..)
  , BotToken(..)
  , BotState(..)
  , BotConfig(..)
  , BotException(..)
  , getUser
  , getChannelMembers
  , sendEphemeralMessage
  , sendMessage
  , retrieveOneMessage
  , startModal
  , updateModal
  , retrieveOneMessageFromThread
  ) where

import Universum

import Control.Monad.Except (throwError)
import Data.Aeson
import Data.Text.Encoding qualified as T
import Servant ((:<|>)(..))
import Servant.Auth.Client qualified as Auth
import Servant.Client
  (BaseUrl(BaseUrl), ClientM, Scheme(Https), client, hoistClient, mkClientEnv, runClientM)

import Text.Interpolation.Nyan
import TzBot.Config
import TzBot.RunMonad
import TzBot.Slack.API

-- | Get a user's info.
getUser :: UserId -> BotM User
getUser userId = do
  token <- getBotToken
  usersInfo token userId >>= endpointFailed "users.info"

-- | Get a list of a channel's members.
getChannelMembers :: ChannelId -> BotM [UserId]
getChannelMembers channelId = do
  token <- getBotToken
  let limit = Limit {limitQ = 200}
  conversationMembers token channelId limit >>= endpointFailed "conversations.members"

-- | Post an "ephemeral message", a message only visible to the given user.
sendEphemeralMessage :: PostEphemeralReq -> BotM ()
sendEphemeralMessage req = do
  token <- getBotToken
  void $ postEphemeral token req >>= endpointFailed "chat.postEphemeral"

-- | Post a message to a given channel.
sendMessage :: PostMessageReq -> BotM ()
sendMessage req = do
  token <- getBotToken
  void $ postMessage token req >>= endpointFailed "chat.postMessage"

-- | Get a message by its id and channel id
retrieveOneMessage :: ChannelId -> MessageId -> BotM Message
retrieveOneMessage channelId messageId = do
  token <- getBotToken
  let endpointName = "conversations.history"
      functionName = "retrieveOneMessage"
  let inclusive = Just True
      limit = Just $ Limit 1
      oldest = Just messageId
      latest = Nothing :: Maybe MessageId
  msgs <- conversationHistory token channelId inclusive limit oldest latest
    >>= endpointFailed "conversations.history"
  case safeHead msgs of
    Just msg -> pure msg
    Nothing ->
      throwError $
        UnexpectedResult endpointName functionName
          $ mkErrorMessage messageId Nothing

-- | Get a message by its id, channel id and thread id
retrieveOneMessageFromThread
  :: ChannelId
  -> ThreadId
  -> MessageId
  -> BotM Message
retrieveOneMessageFromThread channelId threadId messageId = do
  token <- getBotToken
  let endpointName = "conversations.replies"
      functionName = "retrieveOneMessageFromThread"
      inclusive = Just True
      limit = Just $ Limit 1
      oldest = Just messageId
      latest = Nothing :: Maybe MessageId
  msgs <- conversationReplies token channelId threadId inclusive limit oldest latest
    >>= endpointFailed endpointName
  -- For some reason, even if the limit equals to 1, this method
  -- returns the required thread reply together with the original
  -- channel message (-_-)
  case find (\m -> mMessageId m == messageId) msgs of
    Just msg -> pure msg
    Nothing ->
      throwError $
        UnexpectedResult endpointName functionName
          $ mkErrorMessage messageId $ Just threadId

mkErrorMessage :: MessageId -> Maybe ThreadId -> ErrorDescription
mkErrorMessage messageId mbThreadId = do
  let threadAddition =
        fromMaybe "" $
          fmap (\tId -> [int|| and threadId=#{tId}|]) mbThreadId :: Text
  [int||expected to find the message \
        with searched id=#{messageId}#{threadAddition}|]

-- | Start dialog with user.
startModal :: OpenViewReq -> BotM ()
startModal req = do
  token <- getBotToken
  void $ openView token req >>= endpointFailed "views.open"

-- | Proceed dialog with user.
updateModal :: UpdateViewReq -> BotM ()
updateModal req = do
  token <- getBotToken
  void $ updateView token req >>= endpointFailed "views.update"

getBotToken :: BotM Auth.Token
getBotToken = do
  BotToken bt <- asks $ bcBotToken . bsConfig
  pure $ Auth.Token $ T.encodeUtf8 bt

endpointFailed :: Text -> SlackResponse key a -> BotM a
endpointFailed endpoint = \case
  SRSuccess a -> pure a
  SRError err metadata -> do
    log' [int||#{endpoint} error: #{err}; metadata: #{(show metadata :: Text)}|]
    throwError $ EndpointFailed endpoint err

----------------------------------------------------------------------------
-- Endpoints
----------------------------------------------------------------------------

usersInfo :: Auth.Token -> UserId -> BotM (SlackResponse "user" User)
conversationMembers
  :: Auth.Token -> ChannelId -> Limit
  -> BotM (SlackResponse "members" [UserId])
postEphemeral
  :: Auth.Token
  -> PostEphemeralReq
  -> BotM (SlackResponse "message_ts" Value)
postMessage
  :: Auth.Token
  -> PostMessageReq
  -> BotM (SlackResponse "ts" MessageId)

conversationHistory
  :: Auth.Token
  -> ChannelId
  -> Maybe Bool
  -> Maybe Limit
  -> Maybe MessageId
  -> Maybe MessageId
  -> BotM (SlackResponse "messages" [Message])

conversationReplies
  :: Auth.Token
  -> ChannelId
  -> ThreadId
  -> Maybe Bool
  -> Maybe Limit
  -> Maybe MessageId
  -> Maybe MessageId
  -> BotM (SlackResponse "messages" [Message])


openView
  :: Auth.Token -> OpenViewReq -> BotM (SlackResponse "view" Value)
updateView
  :: Auth.Token -> UpdateViewReq -> BotM (SlackResponse "view" Value)

usersInfo
  :<|> conversationMembers
  :<|> postEphemeral
  :<|> postMessage
  :<|> conversationHistory
  :<|> conversationReplies
  :<|> openView
  :<|> updateView =
  hoistClient api naturalTransformation (client api)
  where
    baseUrl = BaseUrl Https "slack.com" 443 "api"

    naturalTransformation :: ClientM a -> BotM a
    naturalTransformation act = BotM do
      manager <- asks bsManager
      let clientEnv = mkClientEnv manager baseUrl
      liftIO (runClientM act clientEnv) >>= \case
        Right a -> pure a
        Left clientError -> throwError $ ServantError clientError
