module TzBot.Slack.SocketMode.Client
  ( wsMain
  ) where

import Control.Lens
import Data.Aeson (Value, decode')
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Text.Strict.Lens (unpacked, utf8)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Socket (HostName, PortNumber)
import Network.WebSockets (ClientApp, receiveData)
import Text.Interpolation.Nyan
import TzBot.Slack.WebAPI.Class qualified as Slack
import TzBot.Slack.WebAPI.Impl (WebAPIConfig, WebAPIState(..))
import TzBot.Slack.WebAPI.Impl qualified as Slack
import URI.ByteString
import Wuss (runSecureClient)

wsApp :: ClientApp ()
wsApp conn = do
  msg <- receiveData conn
  case decode' @Value msg of
    Nothing -> undefined
    Just json ->
      putStrLn $ TL.unpack $ TL.decodeUtf8 $ encodePretty json

  wsApp conn

wsMain :: WebAPIConfig -> IO ()
wsMain webApiConfig = do
  manager <- newManager tlsManagerSettings
  let webApiState = WebAPIState webApiConfig manager

  -- Ask Slack to generate a wss URL for us to connect to.
  wsURI <- Slack.runOrThrowWebAPIM webApiState do
    Slack.genWebSocketsURI

  (host, port, pathAndQuery) <- splitURI wsURI

  runSecureClient host port pathAndQuery \conn -> do
    putStrLn "Established websockets connection"
    wsApp conn

-- | Splits an URI into the components needed to establish
-- a secure websockets connection.
splitURI :: URI -> IO (HostName, PortNumber, String)
splitURI uri = do
  let
    hostMb = uri ^? authorityL . _Just . authorityHostL . hostBSL . utf8 . unpacked
    port =
      fromMaybe 443 $
        uri ^? authorityL . _Just . authorityPortL . _Just . portNumberL
    pathAndQuery =
      (uri ^. pathL . utf8 . unpacked)
      <>
      (uri ^. queryL . to (serializeQuery' rfc3986Normalization) . utf8 . unpacked)
  case hostMb of
    Nothing -> fail
      [int|s|
        The websockets URI did not contain a host:
          #s{uri}
      |]
    Just host ->
      pure (host, fromIntegral @Int @PortNumber port, pathAndQuery)
