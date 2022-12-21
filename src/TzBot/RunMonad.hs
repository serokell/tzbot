module TzBot.RunMonad where

import Universum

import Control.Monad.Except (MonadError)
import Network.HTTP.Client (Manager)
import Servant.Client (ClientError)
import Text.Interpolation.Nyan

newtype AppLevelToken = AppLevelToken { unAppLevelToken :: Text }
newtype BotToken = BotToken { unBotToken :: Text }

data BotConfig = BotConfig
  { wacAppLevelToken :: AppLevelToken
  , wacBotToken :: BotToken
  }

data BotState = BotState
  { wasConfig :: BotConfig
  , wasManager :: Manager
  }

newtype BotM a = BotM
  { unBotM :: ReaderT BotState (ExceptT BotException IO) a
  }
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader BotState, MonadError BotException
    )

runBotM :: BotState -> BotM a -> IO (Either BotException a)
runBotM state webApiM =
  webApiM
    & unBotM
    & flip runReaderT state
    & runExceptT

runOrThrowBotM :: BotState -> BotM a -> IO a
runOrThrowBotM state webApiM =
  runBotM state webApiM >>= either throwM pure

----------------------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------------------

data BotException
  = EndpointFailed Text Text
  | ServantError ClientError
  deriving stock Show

instance Exception BotException where
  displayException = \case
    EndpointFailed endpoint err ->
      [int|s|
        '#{endpoint}' failed:
          #{err}
      |]
    ServantError clientError ->
      displayException clientError
