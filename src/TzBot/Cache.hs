-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Cache
  ( -- * Types
    RandomizedCache

    -- * Cache creation
  , withRandomizedCache
  , withRandomizedCacheDefault

    -- * Altering cache
  , insertRandomized
  , fetchWithCacheRandomized
  , update
  ) where

import Universum

import Control.Concurrent.Async.Lifted (withAsync)
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.Cache.Internal qualified as CacheI
import Data.HashMap.Strict qualified as HM
import System.Clock (TimeSpec)
import Time (Hour, KnownDivRat, Nanosecond, Time(..), hour, threadDelay, toUnit)

import TzBot.Util (randomTimeSpec, timeToTimespec, (+-))

-- | This datatype uses `Cache` datatype inside, but also
--   provides a possibility to slightly vary expiration
--   time randomly around the configured mean value.
--   Use it when you don't want a bunch of cached data
--   to expire at once.
--
--   Automatic periodical cleaning is also included.
data RandomizedCache k v = RandomizedCache
  { rcCache  :: Cache k v
  , rcExpiry :: TimeSpec
  , rcExpiryRandomAmplitude :: TimeSpec
  }

-- | Like `withRandomizedCache` but with reasonable defaults.
-- Expiry random amplitude is 15% of the expiry time,
-- default cleaning period of 24 hours is used.
withRandomizedCacheDefault
  :: ( KnownDivRat u Nanosecond
     , Eq k, Hashable k
     )
  => Time u -- ^ Expiration time
  -> (RandomizedCache k v -> IO a) -- ^ Action that uses the cache
  -> IO a
withRandomizedCacheDefault expiry =
  withRandomizedCache expiry (get15PerCent expiry) Nothing
  where
    get15PerCent :: Time k -> Time k
    get15PerCent (Time r) = Time $ r * 0.15

-- | Create randomized cache and run the passed action with it.
-- Raises `error` on incorrect use.
withRandomizedCache
  :: ( KnownDivRat u Nanosecond
     , Eq k, Hashable k
     , HasCallStack
     )
  => Time u -- ^ Expiration time
  -> Time u -- ^ Expiration time amplitude (should be less than expiration time)
  -> Maybe (Time Hour) -- ^ How frequently to purge all expired items
  -> (RandomizedCache k v -> IO a) -- ^ Action that uses the cache
  -> IO a
withRandomizedCache
    (timeToTimespec -> rcExpiry)
    (timeToTimespec -> rcExpiryRandomAmplitude)
    cleaningPeriod
    action
  = do
  when (rcExpiry - rcExpiryRandomAmplitude <= 0) $
    error "Expiry random amplitude should be lower than the expiry"
  rcCache <- Cache.newCache $ Just rcExpiry
  withAsync
    (cleaningThread (toUnit <$> cleaningPeriod) rcCache)
    (\_ -> action RandomizedCache {..})

defaultCleaningPeriod :: Time Hour
defaultCleaningPeriod = hour 24

cleaningThread :: (Eq k, Hashable k) => Maybe (Time Hour) -> Cache k v -> IO ()
cleaningThread mbCleaningPeriod cache = forever $ do
  Time.threadDelay cleaningPeriod
  Cache.purgeExpired cache
  where
    cleaningPeriod = fromMaybe defaultCleaningPeriod mbCleaningPeriod

-- | Generate a random expiry time and insert a key/value pair into
-- the cache with that expiry time.
insertRandomized
  :: (Eq k, Hashable k, MonadIO m)
  => k
  -> v
  -> RandomizedCache k v
  -> m ()
insertRandomized key val RandomizedCache {..} = do
  let (minTimeSpec, maxTimeSpec) = rcExpiry +- rcExpiryRandomAmplitude
  rndVal <- liftIO $ randomTimeSpec (minTimeSpec, maxTimeSpec)
  liftIO $ Cache.insert' rcCache (Just rndVal) key val

-- | Try to get a value by the key from the cache, delete if it is expired.
-- If the value is either absent or expired, perform given fetch action
-- and insert the obtained value with configured expiration parameters
-- into the cache.
fetchWithCacheRandomized
  :: (Eq k, Hashable k, MonadIO m)
  => k
  -> (k -> m v)
  -> RandomizedCache k v
  -> m v
fetchWithCacheRandomized key fetchAction cache = do
  mv <- liftIO $ Cache.lookup (rcCache cache) key
  case mv of
    Just v -> pure v
    Nothing -> do
      v <- fetchAction key
      insertRandomized key v cache
      pure v

-- | Update the value by the key, expiration is not taken into account.
update
  :: forall k v m. (Eq k, Hashable k, MonadIO m)
  => k
  -> (v -> v)
  -> RandomizedCache k v
  -> m ()
update key valFunc RandomizedCache {..} = do
  atomically $ modifyTVar' (CacheI.container rcCache) $ \hm ->
    HM.adjust itemFunc key hm
  where
    itemFunc :: CacheI.CacheItem v -> CacheI.CacheItem v
    itemFunc ci = ci { CacheI.item = valFunc $ CacheI.item ci }
