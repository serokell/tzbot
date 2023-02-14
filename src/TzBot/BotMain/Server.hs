-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.BotMain.Server where

import Universum

import Control.Monad.Managed (runManaged)
import Data.Aeson (FromJSON(..), Value)
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Data.Aeson.Types (parseEither)
import Network.Wai.Handler.Warp (defaultSettings)
import Network.Wai.Handler.Warp qualified as Warp
import Servant
  (Application, FormUrlEncoded, Handler(Handler), Header, JSON, NoContent(..), PlainText, Post,
  ReqBody, type (:>))
import Servant.API.Generic ((:-))
import Servant.Server.Generic (genericServeT)
import Slacker (SlashCommand)
import Slacker.SocketMode (EventWrapper)
import Text.Interpolation.Nyan (int, rmode', rmode's)
import UnliftIO (async, try)
import Web.FormUrlEncoded (FromForm(..), genericFromForm)

import TzBot.BotMain.Common (withBotState)
import TzBot.BotMain.Server.Extractors
  (pattern BlockActionServer, pattern EventValueServer, pattern InteractiveServer)
import TzBot.Config (Config(..), readConfig)
import TzBot.Logger (logError)
import TzBot.Options (RunServerOptions(..))
import TzBot.ProcessEvents
  (handleRawBlockAction, handleRawEvent, handleRawInteractive, handleSlashCommand)
import TzBot.RunMonad (BotM, BotState, runBotM)
import TzBot.Util (defaultFromFormOptions)

type ReqIdHeader = Header "X-Slack-Request-Timestamp" Text

data Routes mode = Routes
  { rCommon :: mode
    :- ReqIdHeader
      :> ReqBody '[JSON] Value
      :> Post '[PlainText] NoContent
  , rHelp   :: mode
    :- "help"
      :> ReqBody '[FormUrlEncoded] SlashCommand
      :> Post '[PlainText] NoContent
  , rInteractive :: mode
    :- "interactive"
      :> ReqIdHeader
      :> ReqBody '[FormUrlEncoded] InteractiveRequest
      :> Post '[PlainText] NoContent
  } deriving stock (Generic)

runServer :: RunServerOptions -> IO ()
runServer opts = do
  let mbConfigFilePath = rsoConfigFile opts
  bsConfig <- readConfig mbConfigFilePath
  let port = cPort bsConfig
  putStrLn @Text [int||Running on port #{port}|]
  let settings = Warp.setPort port defaultSettings
  runManaged do
    botState <- withBotState bsConfig
    liftIO $ Warp.runSettings settings $ app botState
  where
    app :: BotState -> Application
    app bState = genericServeT (naturalTransformation bState) Routes
      { rCommon = handleEvent
      , rHelp = handleCommand
      , rInteractive = handleInteractive
      }

-- | Here we never report any errors to Slack so never return `ServerError`
naturalTransformation :: BotState -> BotM a -> Handler a
naturalTransformation botState action = Handler $ lift $ runBotM botState action

----------------------------------------------------------------------------
---- Subscribed events
----------------------------------------------------------------------------
handleEvent :: Maybe Text -> Value -> BotM NoContent
handleEvent mbReqTimestamp val = forkAndReturnAck $ do
  let logTag = fromMaybe "unknown" mbReqTimestamp
  let eventWrapper = parseEither parseJSON val :: Either String EventWrapper
  case eventWrapper of
    Left err -> do
      logError [int||Unrecognized EventWrapper: #{err}|]
      logError [int||Full EventWrapper value: #{encodePrettyToTextBuilder val}|]
    Right ew -> case ew of
      EventValueServer typ val -> handleRawEvent logTag typ val
      _ -> logError [int||Invalid Event: #s{ew}|]

----------------------------------------------------------------------------
---- Interactive (including block actions)
----------------------------------------------------------------------------
newtype InteractiveRequest = InteractiveRequest
  { irPayload :: Value
  } deriving stock (Generic)

instance FromForm InteractiveRequest where
  fromForm = genericFromForm defaultFromFormOptions

handleInteractive :: Maybe Text -> InteractiveRequest -> BotM NoContent
handleInteractive mbReqTimestamp req = forkAndReturnAck do
  let logTag = fromMaybe "unknown" mbReqTimestamp
      intValue = irPayload req
  case intValue of
    BlockActionServer actionId blockActionRaw ->
      handleRawBlockAction logTag actionId blockActionRaw
    InteractiveServer typ interactiveRaw ->
      handleRawInteractive logTag typ interactiveRaw
    _ -> logError [int||Unrecognized interactive event: #{encodePrettyToTextBuilder intValue}|]

----------------------------------------------------------------------------
---- Commands
----------------------------------------------------------------------------
handleCommand :: SlashCommand -> BotM NoContent
handleCommand slashCmd = forkAndReturnAck $ handleSlashCommand slashCmd

----------------------------------------------------------------------------
---- Common
----------------------------------------------------------------------------

-- | Slack advices to send ack response as soon as possible, so we run the actual
-- handler in a separate async (without caring about its further destiny)
forkAndReturnAck :: BotM () -> BotM NoContent
forkAndReturnAck action = do
  -- Here we only log sync exceptions,
  -- let the servant decide how to handle others
  let logExceptionWrapper :: BotM () -> BotM ()
      logExceptionWrapper a = do
        eithRes <- UnliftIO.try @_ @SomeException a
        whenLeft eithRes \e ->
          logError [int||Error occured: #{displayException e}|]
  UnliftIO.async $ logExceptionWrapper action
  pure NoContent
