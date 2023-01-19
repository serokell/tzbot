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
  , BotException(..)
  , getUserCached
  , getChannelMembersCached
  , sendEphemeralMessage
  , handleTooManyRequests
  ) where

import Universum hiding (toString)

import Control.Concurrent (threadDelay)
import Control.Monad.Except (throwError)
import Data.Aeson (Value)
import Data.ByteString.UTF8 (toString)
import Data.DList qualified as DList
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.HTTP.Types.Status (Status(statusCode))
import Servant ((:<|>)(..))
import Servant.Auth.Client qualified as Auth
import Servant.Client
  (BaseUrl(BaseUrl), ClientM, Scheme(Https), client, hoistClient, mkClientEnv, runClientM)
import Servant.Client.Core
  (ClientError(FailureResponse), ResponseF(responseHeaders, responseStatusCode))

import TzBot.Cache qualified as Cache
import TzBot.Config (AppLevelToken(..), BotToken(..), Config(..))
import TzBot.RunMonad (BotException(..), BotM(..), BotState(..), log', runBotM, runOrThrowBotM)
import TzBot.Slack.API
  (ChannelId, Cursor, Limit(..), SlackContents(..), SlackResponse(..), ThreadId, User, UserId, api)

-- | Get a user's info.
getUser :: UserId -> BotM User
getUser userId = do
  token <- getBotToken
  usersInfo token userId >>= handleSlackErrorSingle "users.info"

-- | Get a user's info using cache.
getUserCached :: UserId -> BotM User
getUserCached userId =
  asks bsUserInfoCache
    >>= Cache.fetchWithCacheRandomized userId getUser

-- | Get a list of a channel's members.
getChannelMembers :: ChannelId -> BotM (S.Set UserId)
getChannelMembers channelId = do
  token <- getBotToken
  let limit = Limit {limitQ = 200}
  fmap S.fromList
    $ getPaginatedObjects "conversation.members"
    $ conversationMembers token channelId limit

getChannelMembersCached :: ChannelId -> BotM (S.Set UserId)
getChannelMembersCached channelId =
  asks bsConversationMembersCache
    >>= Cache.fetchWithCacheRandomized channelId getChannelMembers

-- | Post an "ephemeral message", a message only visible to the given user.
sendEphemeralMessage :: ChannelId -> Maybe ThreadId -> Text -> UserId -> BotM ()
sendEphemeralMessage channelId threadId text userId = do
  token <- getBotToken
  void $ postEphemeral token userId channelId threadId text >>= handleSlackErrorSingle "chat.postEphemeral"

getBotToken :: BotM Auth.Token
getBotToken = do
  BotToken bt <- asks $ cBotToken . bsConfig
  pure $ Auth.Token $ T.encodeUtf8 bt

handleSlackError :: Text -> SlackResponse a -> BotM a
handleSlackError endpoint = \case
  SRSuccess a -> pure a
  SRError err -> do
    log' $ endpoint <> " error: " <> T.pack (show err)
    throwError $ EndpointFailed endpoint err

handleSlackErrorSingle :: Text -> SlackResponse $ SlackContents key a -> BotM a
handleSlackErrorSingle endpoint = fmap scContents . handleSlackError endpoint

getPaginatedObjects :: Text -> (Maybe Cursor -> BotM $ SlackResponse $ SlackContents key [a]) -> BotM [a]
getPaginatedObjects endpoint action = DList.toList <$> go mempty Nothing
  where
    go acc mbCursor = do
      eithRes <- action mbCursor
      res <- handleSlackError endpoint eithRes
      let newAcc = acc <> DList.fromList (scContents res)
      case scCursor res of
        Nothing -> pure newAcc
        newMbCursor@(Just _) -> go newAcc newMbCursor

----------------------------------------------------------------------------
-- Endpoints
----------------------------------------------------------------------------

usersInfo :: Auth.Token -> UserId -> BotM (SlackResponse $ SlackContents "user" User)
conversationMembers
  :: Auth.Token -> ChannelId -> Limit -> Maybe Cursor
  -> BotM (SlackResponse $ SlackContents "members" [UserId])
postEphemeral
  :: Auth.Token -> UserId -> ChannelId -> Maybe ThreadId -> Text
  -> BotM (SlackResponse $ SlackContents "message_ts" Value)

usersInfo
  :<|> conversationMembers
  :<|> postEphemeral =
  hoistClient api naturalTransformation (client api)
  where
    baseUrl = BaseUrl Https "slack.com" 443 "api"

    naturalTransformation :: ClientM a -> BotM a
    naturalTransformation act = BotM do
      manager <- asks bsManager
      config <- asks bsConfig
      let clientEnv = mkClientEnv manager baseUrl
      liftIO (evalStateT (handleTooManyRequests (runClientM act clientEnv)) (cMaxRetries config)) >>= \case
        Right a -> pure a
        Left clientError -> throwError $ ServantError clientError

-- | Handles slack API response with status code 429 @Too many requests@.
-- If action result is success, then return result. If action result is error
-- with response status code 429 and attempts are not exceeded then try the action again.
handleTooManyRequests :: IO (Either ClientError a) -> StateT Int IO (Either ClientError a)
handleTooManyRequests botAction = do
  result <- liftIO botAction
  case result of
    Right _ -> return result
    Left err -> handle err
  where
    handle err = case err of
        FailureResponse _ response ->
          if ((statusCode . responseStatusCode $ response) == 429)
          -- TODO: Add logging when handling 429 response status code
            then do
              attempts <- get
              if attempts > 0
                then do
                  let retryAfterHeader =
                        Seq.filter ((== "Retry-After") . fst) (responseHeaders response)
                  if Seq.null retryAfterHeader
                    then return $ Left err -- if there is no Retri-After header don't handle 429 response
                    else do
                      let retryAfter =
                            fmap (ceiling . (* 10^(6 :: Int)))
                                (readMaybe $
                                  toString . snd $
                                  Seq.index retryAfterHeader 0 :: Maybe Double)
                      maybe (pure $ Left err) (waitAndTryAgain botAction) retryAfter
                else return $ Left err -- if number of attempts are exceeded don't try anymore
            else return $ Left err -- if response code is other than 429 don't do anything
        _ -> return $ Left err -- if servant response is other than FailureResponse don't do anything
    waitAndTryAgain :: IO (Either ClientError a) -> Int -> StateT Int IO (Either ClientError a)
    waitAndTryAgain action delay = do
      liftIO . threadDelay $ delay
      modify (subtract 1)
      handleTooManyRequests action
