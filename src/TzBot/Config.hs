-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Config (
  readConfig,
  module Types,

  -- * These two are needed only for tests
  readConfigWithEnv,
  LoadConfigError (..)
  ) where

import Universum hiding (lift)

import Control.Exception (evaluate, handle)
import Data.List (singleton)
import Data.Map qualified as M
import Data.String.Conversions
import Data.Validation
import Data.Yaml (FromJSON, decodeEither', toJSON)
import Data.Yaml qualified as Y
import Data.Yaml.Config (ignoreEnv, loadYamlSettings)
import Fmt
import System.Environment qualified as Env
import System.IO.Unsafe (unsafePerformIO)
import Text.Interpolation.Nyan

import TzBot.Config.Default
import TzBot.Config.Types as Types
import TzBot.Instances ()

data LoadConfigError =
  LCEBothEnvAndConfigFieldMissing FieldName EnvVarName
  | LCEEnvVarParseError EnvVarName String
  | LCEYamlParseError Y.ParseException
  | LCEInvalidValue FieldName String
  deriving stock (Show)

instance Buildable LoadConfigError where
  build = \case
    LCEBothEnvAndConfigFieldMissing fieldName envVar ->
        [int||Required field #{fieldName} not found in \
              the config and the envvar #{envVar} is unset.
          |]
    LCEEnvVarParseError envVar err -> [int||#{envVar}: #{show @Text err}|]
    LCEYamlParseError yExc -> fromString $ Y.prettyPrintParseException yExc
    LCEInvalidValue fieldName desc -> [int||Invalid value of #{fieldName}.\
                                            #{desc}.
                                        |]

-- This prevents Config.Default.defaultConfigText to be incorrect on compiling.
defaultConfig :: Config 'CSInterm
defaultConfig =
  $(fst defaultConfigTrick)
   (snd defaultConfigTrick)

parseEnvField
  :: forall a. (FromJSON a)
  => Env
  -> EnvVarName
  -> Either LoadConfigError (Maybe a)
parseEnvField env envVar = do
  let mkErr x = LCEEnvVarParseError envVar $ show x
  let mbRaw = M.lookup envVar env
  case mbRaw of
    Nothing -> Right Nothing
    Just raw -> Just <$> first mkErr (decodeEither' $ cs raw)

-- | Given a value read from the config files, try to override it with
-- environment variable data. If neither environment variable is set and is correct
-- nor the field present in the configs, emit an error.
parseEnvFieldRequiredOverriding
  :: forall a. (FromJSON a)
  => Env
  -> FieldName
  -> EnvVarName
  -> Maybe a
  -> Either LoadConfigError a
parseEnvFieldRequiredOverriding env fieldName envVar mbPrev = do
  mbVal <- parseEnvField env envVar
  let err = LCEBothEnvAndConfigFieldMissing fieldName envVar
  case mbVal of
    Nothing -> maybe (Left err) Right mbPrev
    Just val -> Right val

toValidation :: Either e a -> Validation [e] a
toValidation = (fromEither . first singleton)

readConfigWithEnv
  :: Env
  -> Maybe FilePath
  -> IO $ Either [LoadConfigError] (Config 'CSFinal)
readConfigWithEnv env mbPath =
  -- We use `unsafePerformIO` inside a pure function, so we want to
  -- trigger it if it's going to be really used. Our @Config@ datatype
  -- has strict fields (because of the @StrictData@ extension), so `evaluate`
  -- causes all of its fields to be evaluated at least to the WHNF.
  -- After this, we catch yaml exception in order to pretty-print it later.
  handle handleFunc $ traverse evaluate $ toEither $ do
  let cfg :: Config 'CSInterm
      -- The most easy way to read file on demand, acceptable here.
      cfg = unsafePerformIO $
        loadYamlSettings (maybeToList mbPath) [toJSON defaultConfig] ignoreEnv
  cAppToken <- toValidation $
    parseEnvFieldRequiredOverriding env "appToken" appTokenEnv (cAppToken cfg)
  cBotToken <- toValidation $
    parseEnvFieldRequiredOverriding env "botToken" botTokenEnv (cBotToken cfg)
  cMaxRetries <- (fromMaybe (cMaxRetries cfg) <$>
    toValidation (parseEnvField env maxRetriesEnv)) `bindValidation` validateMaxTries
  cCacheUsersInfo <- fromMaybe (cCacheUsersInfo cfg) <$>
    toValidation (parseEnvField env cacheUsersEnv)
  cCacheConversationMembers <- fromMaybe (cCacheConversationMembers cfg) <$>
    toValidation (parseEnvField env cacheConvMembersEnv)
  pure $ Config {..}
  where
    handleFunc :: Y.ParseException -> IO (Either [LoadConfigError] $ Config 'CSFinal)
    handleFunc = pure . Left . singleton . LCEYamlParseError
    validateMaxTries :: Int -> Validation [LoadConfigError] Int
    validateMaxTries v = validate ([LCEInvalidValue "maxRetries" "Must be in range from 0 to 3"]) check v
      where
        check :: Int -> Maybe Int
        check v =
          if v < 0 || v > 3
            then Nothing
            else Just v

-- | Read config from environment, given filepath, filling up missing fields
-- with default values. Priorities:
-- 1. Environment variable;
-- 2. Configuration file;
-- 3. Default settings.
--
-- If all the configuration is available in the environment variables,
-- configuration file is not touched (i.e. it may not exist).
readConfig :: Maybe FilePath -> IO (Config 'CSFinal)
readConfig mbPath = do
  let prepend = ("Failed to load configuration:" :)
  environment <- M.fromList <$> Env.getEnvironment
  eithEnvConfig <- readConfigWithEnv environment mbPath
  either (\es -> mapM_ (hPutStrLn stderr) (prepend $ map (pretty @_ @String) es) >> exitFailure) pure eithEnvConfig
