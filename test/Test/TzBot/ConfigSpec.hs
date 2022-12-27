-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.TzBot.ConfigSpec (
  test_configSpec
  ) where

import Universum

import Data.Map qualified as M
import Test.Hspec (it, shouldBe, shouldSatisfy)
import Test.Tasty hiding (after_)
import Test.Tasty.Hspec

import TzBot.Config
import TzBot.Config.Default

test_configSpec :: IO TestTree
test_configSpec = testGroup "Config" <$> sequence [defaultConfigSpec, configLoadingSpec]

defaultConfigSpec :: IO TestTree
defaultConfigSpec =
  testSpec "Default config" $ do
    it "config/config.yaml should be the same as the default config" $ do
      contents <- (readFile "config/config.yaml" :: IO Text)
      contents `shouldBe` decodeUtf8 defaultConfigText

configLoadingSpec :: IO TestTree
configLoadingSpec =
  testSpec "Config loading" $ do
    it "should not touch the config file if all envvars are set" $ do
      let env = M.fromList $
            [ (appTokenEnv, "app-token")
            , (botTokenEnv, "bot-token")
            , (maxRetriesEnv, "3")
            , (cacheUsersEnv, "3m")
            , (cacheConvMembersEnv, "3m")
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
    it "should report all the missing required fields/envvars and the envvars with incorrect contents" $ do
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
    it "should succeed with the valid config file and the environment containing only secrets" $ do
      let env = M.fromList $
            [ (botTokenEnv, "bot-token")
            , (appTokenEnv, "app-token")
            ]
      eithConfig <- readConfigWithEnv env (Just "config/config.yaml")
      eithConfig `shouldSatisfy` isRight
