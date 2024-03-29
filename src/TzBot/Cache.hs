-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Cache
  ( -- * Types
    TzCache

    -- * Cache creation
  , TzCacheSettings (..)
  , defaultTzCacheSettings
  , withTzCache
  , withTzCacheDefault

    -- * Inspect
  , lookup

    -- * Altering cache
  , insert
  , fetchWithCache
  , update
  ) where

import TzPrelude

import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.Cache.Internal qualified as CacheI
import Data.HashMap.Strict qualified as HM
import Formatting (Buildable)
import System.Clock (TimeSpec)
import Text.Interpolation.Nyan (int, rmode')
import Time (Hour, KnownDivRat, Nanosecond, Time(..), hour, threadDelay, toUnit)
import UnliftIO.Async (withAsync)

import TzBot.Logger (KatipContext, katipAddNamespace, logDebug)
import TzBot.Util (multTimeSpec, randomTimeSpec, timeToTimespec, (+-))

-- | This datatype uses `Cache` datatype inside, but also
--   provides a possibility to slightly vary expiration
--   time randomly around the configured mean value.
--   Use it when you don't want a bunch of cached data
--   to expire at once. Also can be used without randomizing.
--
--   Automatic periodical cleaning is also included.
data TzCache k v = TzCache
  { tcCache  :: Cache k v
  , tcExpiry :: TimeSpec
  , tcExpiryRandomAmplitude :: Maybe TimeSpec
  }

data TzCacheSettings = TzCacheSettings
  { tcsExpiryRandomAmplitudeFraction :: Maybe Double
    -- ^ Should be 0 < f < 1, `Nothing` means no randomizing.
  , tcsCleaningPeriod :: Time Hour
    -- ^ How frequently to purge expired items.
  }

defaultTzCacheSettings :: TzCacheSettings
defaultTzCacheSettings = TzCacheSettings
  { tcsExpiryRandomAmplitudeFraction = Nothing -- no randomizing
  , tcsCleaningPeriod = hour 24
  }

-- | `withTzCache` with `defaultTzCacheSettings`.
withTzCacheDefault
  :: ( KnownDivRat u Nanosecond
     , Hashable k
     )
  => Time u -- ^ Expiration time
  -> (TzCache k v -> IO a) -- ^ Action that uses the cache
  -> IO a
withTzCacheDefault = withTzCache defaultTzCacheSettings

-- | Create randomized cache and run the passed action with it.
-- Raises `error` on incorrect use, check `TzCacheSettings` for
-- constraints on its fields.
withTzCache
  :: ( KnownDivRat u Nanosecond
     , Hashable k
     , HasCallStack
     )
  => TzCacheSettings -- ^ Settings
  -> Time u -- ^ Expiration time
  -> (TzCache k v -> IO a) -- ^ Action that uses the cache
  -> IO a
withTzCache
    settings
    (timeToTimespec -> tcExpiry)
    action
  = do
  let isRandAmpValid :: Double -> Bool
      isRandAmpValid x = x > 0 && x < 1
  when (fmap isRandAmpValid settings.tcsExpiryRandomAmplitudeFraction == Just False) $
    error "Expiry random amplitude should be <1 and >0"
  let tcExpiryRandomAmplitude = (flip multTimeSpec tcExpiry) <$> settings.tcsExpiryRandomAmplitudeFraction
  tcCache <- Cache.newCache $ Just tcExpiry
  withAsync
    (cleaningThread (toUnit settings.tcsCleaningPeriod) tcCache)
    (\_ -> action TzCache {..})

cleaningThread :: (Hashable k) => Time Hour -> Cache k v -> IO ()
cleaningThread cleaningPeriod cache = forever $ do
  Time.threadDelay cleaningPeriod
  Cache.purgeExpired cache

-- | Generate a random expiry time and insert a key/value pair into
-- the cache with that expiry time.
insert
  :: (Hashable k, MonadIO m)
  => k
  -> v
  -> TzCache k v
  -> m ()
insert key val cache = do
  expiry <- case cache.tcExpiryRandomAmplitude of
    Nothing -> pure cache.tcExpiry
    Just randAmp -> do
      let (minTimeSpec, maxTimeSpec) = cache.tcExpiry +- randAmp
      liftIO $ randomTimeSpec (minTimeSpec, maxTimeSpec)
  liftIO $ Cache.insert' cache.tcCache (Just expiry) key val

-- | Try to get a value by the key from the cache, delete if it is expired.
-- If the value is either absent or expired, perform given fetch action
-- and insert the obtained value with configured expiration parameters
-- into the cache.
fetchWithCache
  :: (Hashable k, KatipContext m, Buildable k)
  => k
  -> (k -> m v)
  -> TzCache k v
  -> m v
fetchWithCache key fetchAction cache =
  katipAddNamespace "cache" $ do
  logDebug [int||Fetching key=#{key}|]
  mv <- liftIO $ Cache.lookup (tcCache cache) key
  case mv of
    Just v -> logDebug "Using cache" >> pure v
    Nothing -> do
      logDebug [int||
        Key #{key} expired or absent: \
        using provided fetching action
        |]
      v <- fetchAction key
      insert key v cache
      pure v

lookup
  :: (Hashable k, MonadIO m)
  => k
  -> TzCache k v
  -> m (Maybe v)
lookup key cache = liftIO $ Cache.lookup cache.tcCache key

-- | Update the value by the key (if the key exists in the cache), expiration is not taken into account.
update
  :: forall k v m. (Hashable k, MonadIO m)
  => k
  -> (v -> v)
  -> TzCache k v
  -> m ()
update key valFunc cache = do
  atomically $ modifyTVar' (CacheI.container cache.tcCache) $ \hm ->
    HM.adjust itemFunc key hm
  where
    itemFunc :: CacheI.CacheItem v -> CacheI.CacheItem v
    itemFunc ci = ci { CacheI.item = valFunc $ CacheI.item ci }
