-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Util where

import Universum

import Data.Aeson (FromJSON(..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Casing (aesonPrefix, camelCase, snakeCase)
import Data.Yaml qualified as Y
import Language.Haskell.TH (Exp, Q)
import System.Clock (TimeSpec, fromNanoSecs, toNanoSecs)
import System.Random (randomRIO)
import Time (KnownDivRat, Nanosecond, Time, floorRat, ns, toUnit)

attach :: (Functor f) => (a -> b) -> f a -> f (a, b)
attach f = fmap (\x -> (x, f x))

aesonStripLowercasePrefixOptions :: Aeson.Options
aesonStripLowercasePrefixOptions = aesonPrefix snakeCase

-- | Options that we use to derive JSON instances for config types.
aesonConfigOptions :: Aeson.Options
aesonConfigOptions = (aesonPrefix camelCase){Aeson.rejectUnknownFields = True}

-- | Usage: $(fst trick) $ snd trick, where (trick :: Trick a)
--   is defined in another module.
type Trick a = (Q Exp, a)

thTrickYaml :: forall a. (FromJSON a) => ByteString -> Trick a
thTrickYaml input = case first show $ Y.decodeEither' input of
  Left str -> (fail str, error "")
  Right c -> ([|id|], c)

timeToTimespec :: KnownDivRat k Nanosecond => Time k -> TimeSpec
timeToTimespec = fromNanoSecs . floorRat . toUnit @Nanosecond

timespecToTime :: KnownDivRat Nanosecond k => TimeSpec -> Time k
timespecToTime = toUnit . ns . fromIntegral . toNanoSecs

{-
>>> import Time
>>> let Just time = unitsP "30m" :: Maybe (Time Minute)
>>> (timespecToTime $ timeToTimespec time :: Time Minute) == time
True
 -}

randomTimeSpec :: (TimeSpec, TimeSpec) -> IO TimeSpec
randomTimeSpec (min, max) =
  fromNanoSecs <$> randomRIO (toNanoSecs min, toNanoSecs max)

(+-) :: Num a => a -> a -> (a, a)
x +- y = (x - y, x + y)
