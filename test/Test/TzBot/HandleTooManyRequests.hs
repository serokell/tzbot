-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.TzBot.HandleTooManyRequests (test_HandleTooManyRequests) where

import TzPrelude

import Data.Fixed (Fixed(MkFixed))
import Data.Sequence qualified as Seq
import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Katip
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Method (methodGet)
import Network.HTTP.Types.Status (status429)
import Network.HTTP.Types.Version (http11)
import Servant.Client.Core
  (BaseUrl(BaseUrl), ClientError(FailureResponse), RequestF(Request), ResponseF(Response),
  Scheme(Https))
import Test.Tasty (TestTree, localOption)

import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.Tasty.QuickCheck
  (Arbitrary(arbitrary), Property, QuickCheckTests(QuickCheckTests), choose, generate, testProperty)
import TzBot.Logger
import TzBot.Slack (handleTooManyRequests)

-- | Delay between attempts in seconds
newtype Delay = Delay {unDelay :: Double} deriving stock (Show)

-- | Number of attempts to call slack api
newtype Attempts = Attempts {unAttempts :: Int} deriving stock (Show)

instance Arbitrary Delay where
  arbitrary = Delay <$> choose (0.5, 1)

instance Arbitrary Attempts where
  arbitrary = Attempts <$> choose (1, 3)

-- | Mutable test state
data TestState = TestState
  {
    attempts :: Int  -- ^ The number of attempts left
  , delay :: Double  -- ^ Sum of delays during every attempt
  }

test_HandleTooManyRequests :: TestTree
test_HandleTooManyRequests =
  localOption (QuickCheckTests 10) $
  testProperty "Handle Too Many Requests" handleTooManyRequestsProperty

-- | Property function of handling of slack API response 429 @Too many request@
-- It works as follows:
--
-- 1. For arbitrary number of attempts it runs `handleTooManyRequests` with mock of
-- slack request computation.
-- 2. Fixes the time the computation takes.
-- 3. The time of computation should be not less the sum of delays in every attempt.
handleTooManyRequestsProperty :: Attempts -> Property
handleTooManyRequestsProperty attempts = monadicIO $ do
  let attempts' = unAttempts attempts
  stateRef <- newIORef $ TestState attempts' 0
  start <- run getCurrentTime
  run $ withLogger DebugS $ \(ns, ctx, le) ->
    flip evalStateT attempts' $
    runKatipContextT le ctx ns $ katipNoLogging $
    handleTooManyRequests $ mockSlackRequest stateRef
  end <- run getCurrentTime
  delay' <- delay <$> readIORef stateRef
  let realDelay = nominalDiffTimeToSeconds $ diffUTCTime end start
      expectedDelay = MkFixed ((ceiling (delay' * 10 ^ (12 :: Int))))
  assert (realDelay >= expectedDelay)

-- | Mock for slack API request.
-- It uses the value of attempts from test state to emulate behavour of slack API.
-- While the number of left attempts is greater than zero it responses with status
-- 429 'Too many requests' with arbitrary @Retry-After@ header value of `Delay`.
-- After this it returns success.
mockSlackRequest :: IORef TestState -> IO (Either ClientError ())
mockSlackRequest stateRef = do
  attempts' <- attempts <$> readIORef stateRef
  if attempts' == 0
    then return $ Right ()
    else do
        delay' <- generate arbitrary :: IO Delay
        modifyIORef
          stateRef
          (\st -> st {attempts = attempts' - 1, delay = delay st + unDelay delay'})
        return $ Left (failureResponse delay')
  where
    failureResponse :: Delay -> ClientError
    failureResponse delay = FailureResponse request response
      where
        request = Request
                (BaseUrl Https "host" 80 "", "")
                Seq.empty
                Nothing
                Seq.empty
                Seq.empty
                http11
                methodGet
                :: RequestF () (BaseUrl, ByteString)
        headers = Seq.singleton ("retry-after", show . unDelay $ delay) :: Seq Header
        response = Response status429 headers http11 ""
