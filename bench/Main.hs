-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Main where

import Universum

--import Control.Concurrent.Async
import Criterion
import Criterion.Main (defaultMain)
import Data.Text qualified as T
import System.Environment (getEnv)
import Text.Read
import Time

import TzBot.Cache qualified as Cache
import TzBot.Cache1 qualified as Cache1
import TzBot.Config.Types (BotConfig, Config (..))
import TzBot.Config (readConfig)
import TzBot.BotMain (withBotState)
import TzBot.Slack.API (ChannelId(ChannelId), UserId (UserId))
import TzBot.RunMonad (runBotM, BotState)
import TzBot.Slack
import Control.Concurrent.Async.Lifted (forConcurrently_, forConcurrently)
import qualified Data.Set as S
import Text.Interpolation.Nyan

benchFunc :: Int -> IO ()
benchFunc num = Cache.withRandomizedCacheDefault @_ (minute 10) $ \cache -> do
  let ints = [1..num]
  forConcurrently_ ints $ \int -> do
    Cache.fetchWithCacheRandomized int pure cache

benchFunc1 :: Int -> IO ()
benchFunc1 num = Cache1.withRandomizedCacheDefault @_ (minute 10) $ \cache -> do
  let ints = [1..num]
  forConcurrently_ ints $ \int -> do
    Cache1.fetchWithCacheRandomized int pure cache

benchUsers :: [UserId] -> BotState -> IO ()
benchUsers usersInChannelIds bState = {-(print =<<) $ -} void $ runBotM bState $ do
  res <- forConcurrently usersInChannelIds getUser
  pure ()
--  print $ length res

benchUsersSeq :: [UserId] -> BotState -> IO ()
benchUsersSeq usersInChannelIds bState = void $ runBotM bState $ do
  res <- forM usersInChannelIds getUser
  pure () -- print $ length res

main :: IO ()
main = do
--  num :: Int <- read <$> getEnv "BENCH"
--  slackChannel <- ChannelId . T.pack <$> getEnv "CHANNEL"
--  config <- readConfig $ Just "/home/yuri/prog/haskell/serokell/tzbot/config/config.yaml"
  defaultMain $ flip map [10, 50, 100, 500, 1000] $ \num ->
    bench [int||cache: #{num} times|] $ whnfIO $ benchFunc1 num
--    , bench "new cache" $ whnfIO $ benchFunc1 num

{-
  withBotState config $ \bState -> void $ runBotM bState $ do
    usersInChannelIds <- S.toList <$> getChannelMembers slackChannel
    liftIO $ defaultMain $
      [ bench "channel users par" $ nfIO $ benchUsers usersInChannelIds bState
      , bench "channel users seq" $ nfIO $ benchUsersSeq usersInChannelIds bState
      ]
-}
