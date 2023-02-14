-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.BotMain.Server.Verification where

import Universum

import Data.Aeson (FromJSON(..), ToJSON, Value)
import Network.Wai.Handler.Warp (defaultSettings)
import Network.Wai.Handler.Warp qualified as Warp
import Servant (Application, Handler, JSON, PlainText, Post, ReqBody, type (:>))
import Servant.API.Generic ((:-))
import Servant.Server.Generic (genericServe)
import Text.Interpolation.Nyan (int, rmode', rmode's)

import TzBot.Config (Config(..), readConfig)
import TzBot.Options (RunServerOptions(..))
import TzBot.Util (RecordWrapper(..))

type API = ReqBody '[JSON] Value :> Post '[PlainText] Text

newtype VerificationRoutes mode = VerificationRoutes
  { vrMain :: mode :- ReqBody '[JSON] VerifyingRequest :> Post '[PlainText] Text
  } deriving stock (Generic)

data VerifyingRequest = VerifyingRequest
  { vrChallenge :: Text
  , vrToken :: Text
  , vrType :: Text
  } deriving stock (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via RecordWrapper VerifyingRequest

-- | When trying to submit a URL for the bot, Slack will send verification
-- request, the bot should just respond with \"challenge\" value.

-- TODO: Slack also should check the server SSL certificates; currently this
-- was just tested with ngrok which has its own certificates, but for production
-- we need our own ones.
runVerificationServer :: RunServerOptions -> IO ()
runVerificationServer opts = do
  let mbConfigFilePath = rsoConfigFile opts
  bsConfig <- readConfig mbConfigFilePath
  let port = cPort bsConfig
  let settings = Warp.setPort port defaultSettings
  putStrLn @Text "Running in verification mode"
  putStrLn @Text [int||Running on port #{port}|]
  Warp.runSettings settings app
  where
    app :: Application
    app = genericServe $ VerificationRoutes handler

handler :: VerifyingRequest -> Handler Text
handler verReq = do
  putStrLn @Text [int||got verification value: #s{verReq}|]
  pure $ vrChallenge verReq
