-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.TzBot.ConfigSpec (
  test_configSpec
  ) where

import TzPrelude

import Data.Map qualified as M
import Test.Hspec (expectationFailure, it, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.Tasty hiding (after_)
import Test.Tasty.Hspec (testSpec)

import TzBot.Config
import TzBot.Config.Default (defaultConfigText)

test_configSpec :: IO TestTree
test_configSpec =
  testGroup "Config" <$> sequence [defaultConfigSpec, configLoadingSpec]

defaultConfigSpec :: IO TestTree
defaultConfigSpec =
  testSpec "Default config" $ do
    it "config/config.yaml should be the same as the default config" $ do
      contents <- (readFile "config/config.yaml" :: IO Text)
      when (contents /= decodeUtf8 defaultConfigText) $
        expectationFailure
          "config/config.yaml should be the same as the default config,\
            \ please run `stack exec tzbot-exe -- dump-config --file\
            \ config/config.yaml --force"

configLoadingSpec :: IO TestTree
configLoadingSpec =
  testSpec "Config loading" $ do
    it "should not touch the config file if all envvars are set" $ do
      let env :: Map String String
          env = M.fromList $
            [ (appTokenEnv, "app-token")
            , (botTokenEnv, "bot-token")
            , (maxRetriesEnv, "3")
            , (cacheUsersEnv, "3m")
            , (cacheConvMembersEnv, "3m")
            , (feedbackChannelEnv, "C13FQHWLQS2")
            , (feedbackFileEnv, "feedback.log")
            , (cacheReportDialogEnv, "3m")
            , (inverseHelpUsageChanceEnv, "15")
            , (logLevelEnv, "Info")
            ]
      eithConfig <- readConfigWithEnv env (Just "config/nonexistent.yaml")
      eithConfig `shouldSatisfy` isRight
    it "should report missing yaml file if SLACK_TZ_BOT_TOKEN is not set" $ do
      let env = M.fromList $
            [ (appTokenEnv, "app-token")
            , (maxRetriesEnv, "3")
            , (cacheUsersEnv, "3m")
            , (cacheConvMembersEnv, "3m")
            ]
      eithConfig <- readConfigWithEnv env (Just "config/nonexistent.yaml")
      eithConfig `shouldSatisfy` \case
        Left [LCEYamlParseError _] -> True
        _ -> False
    it "should report missing yaml file if SLACK_TZ_APP_TOKEN is not set" $ do
      let env = M.fromList $
            [ (botTokenEnv, "bot-token")
            , (maxRetriesEnv, "3")
            , (cacheUsersEnv, "3m")
            , (cacheConvMembersEnv, "3m")
            ]
      eithConfig <- readConfigWithEnv env (Just "config/nonexistent.yaml")
      eithConfig `shouldSatisfy` \case
        Left [LCEYamlParseError _] -> True
        _ -> False
    it "should report all the missing required fields/envvars\
                    \ and the envvars with incorrect contents" $ do
      let env = M.fromList $
            [ (botTokenEnv, "bot-token")
            , (maxRetriesEnv, "mazda")
            , (cacheUsersEnv, "m3")
            , (cacheConvMembersEnv, "3m")
            ]
      eithConfig <- readConfigWithEnv env (Just "config/config.yaml")
      eithConfig `shouldSatisfy` \case
        Left
          [ LCEBothEnvAndConfigFieldMissing "appToken" "SLACK_TZ_APP_TOKEN"
          , LCEEnvVarParseError "SLACK_TZ_MAX_RETRIES" _
          , LCEEnvVarParseError "SLACK_TZ_CACHE_USERS_INFO" _
          ] -> True
        _ -> False
    prop "maxRetries validation" $ \maxRetries -> do
      let env = M.fromList $
            [ (botTokenEnv, "bot-token")
            , (appTokenEnv, "app-token")
            , (maxRetriesEnv, show (maxRetries :: Int))
            ]
      eithConfig <- readConfigWithEnv env (Just "config/config.yaml")
      eithConfig `shouldSatisfy` \case
        Left
          [LCEInvalidValue "maxRetries" "Must be in range from 0 to 3"] -> maxRetries < 0 || maxRetries > 3
        _ -> maxRetries >= 0 && maxRetries <= 3
    it "should succeed with the valid config file and\
                      \ the environment containing only secrets" $ do
      let env = M.fromList $
            [ (botTokenEnv, "bot-token")
            , (appTokenEnv, "app-token")
            ]
      eithConfig <- readConfigWithEnv env (Just "config/config.yaml")
      eithConfig `shouldSatisfy` isRight
