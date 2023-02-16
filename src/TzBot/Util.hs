-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Util where

import Universum hiding (last, try)

import Control.Lens (LensRules, lensField, lensRules, mappingNamer)
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.Casing
import Data.Aeson.Key qualified as AeKey
import Data.Aeson.Types (Parser, parseMaybe)
import Data.CaseInsensitive qualified as CI
import Data.Char (isLower)
import Data.HashMap.Strict qualified as H
import Data.List (stripPrefix)
import Data.String.Conversions (cs)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Data.Yaml qualified as Y
import GHC.Generics
import Language.Haskell.TH
import System.Clock (TimeSpec, fromNanoSecs, toNanoSecs)
import System.Random (randomRIO)
import Text.Interpolation.Nyan (int, rmode')
import Time (KnownDivRat, Nanosecond, Time, floorRat, ns, toUnit)

attach :: (Functor f) => (a -> b) -> f a -> f (a, b)
attach f = fmap (\x -> (x, f x))

withMaybe :: Maybe a -> b -> (a -> b) -> b
withMaybe mbVal nothing just = maybe nothing just mbVal

tsToUTC :: String -> Maybe UTCTime
tsToUTC = parseTimeM False defaultTimeLocale "%s%Q"

parseSlackTimestamp :: AeKey.Key -> String -> Parser UTCTime
parseSlackTimestamp fieldName tsStr = do
  let failMessage = [int||Failed to parse timestamp "#{AeKey.toString fieldName}"|]
  maybe (fail failMessage) pure $ tsToUTC tsStr

fetchSlackTimestamp :: AeKey.Key -> Object -> Parser UTCTime
fetchSlackTimestamp key o = o .: key >>= parseSlackTimestamp key

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

randomTimeSpec :: (TimeSpec, TimeSpec) -> IO TimeSpec
randomTimeSpec (min, max) =
  fromNanoSecs <$> randomRIO (toNanoSecs min, toNanoSecs max)

multTimeSpec :: Double -> TimeSpec -> TimeSpec
multTimeSpec mult =
  fromNanoSecs . floor . (* mult) . fromIntegral . toNanoSecs

(+-) :: Num a => a -> a -> (a, a)
x +- y = (x - y, x + y)

-- aeson utils
decodeMaybe :: FromJSON a => Value -> Maybe a
decodeMaybe = parseMaybe parseJSON

defaultRecordOptions :: Options
defaultRecordOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . dropWhile isLower
  , omitNothingFields = True
  }

newtype RecordWrapper a = RecordWrapper a

instance (Generic a, GToJSON' Value Zero (Rep a)) => ToJSON (RecordWrapper a) where
  toJSON (RecordWrapper x) = genericToJSON defaultRecordOptions x

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (RecordWrapper a) where
  parseJSON val = RecordWrapper <$> genericParseJSON defaultRecordOptions val

----
defaultTypedOptions :: Options
defaultTypedOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . dropWhile isLower
  , constructorTagModifier = camelTo2 '_'
  , tagSingleConstructors = True
  , sumEncoding = TaggedObject "type" "contents"
  , omitNothingFields = True
  }

newtype TypedWrapper a = TypedWrapper a

instance (Generic a, GToJSON' Value Zero (Rep a)) => ToJSON (TypedWrapper a) where
  toJSON (TypedWrapper x) = genericToJSON defaultTypedOptions x

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (TypedWrapper a) where
  parseJSON x = TypedWrapper <$> genericParseJSON defaultTypedOptions x
----
defaultSumOptions :: Options
defaultSumOptions = defaultOptions
  { sumEncoding = UntaggedValue
  }

newtype SumWrapper a = SumWrapper a

instance (Generic a, GToJSON' Value Zero (Rep a)) => ToJSON (SumWrapper a) where
  toJSON (SumWrapper x) = genericToJSON defaultSumOptions x

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (SumWrapper a) where
  parseJSON x = SumWrapper <$> genericParseJSON defaultSumOptions x
----
encodeText :: ToJSON a => a -> Text
encodeText = cs . encode

decodeText :: FromJSON a => Text -> Maybe a
decodeText = decode . cs

----
neConcatMap :: (a -> NonEmpty b) -> NonEmpty a -> NonEmpty b
neConcatMap func ns = foldr1 (<>) $ fmap func ns

{-
>>> neConcatMap (\x -> x :| [x + 1]) $ 1 :| [2,3]
1 :| [2,2,3,3,4]
 -}

----
stripPrefixIfPresent :: Eq a => [a] -> [a] -> [a]
stripPrefixIfPresent pref x = fromMaybe x $ stripPrefix pref x

newtype WithUnknown a = WithUnknown { unUnknown :: Either Value a }
  deriving stock (Show, Eq)

instance (FromJSON a) => FromJSON (WithUnknown a) where
  parseJSON val = WithUnknown <$> asum [Right <$> parseJSON val, pure $ Left val]

instance (ToJSON a) => ToJSON (WithUnknown a) where
  toJSON (WithUnknown eith) = either (String . show) toJSON eith

----
newtype CIStorage a = CIStorage { unCIStorage :: H.HashMap (CI.CI Text) a }

fromList :: [(Text, a)] -> CIStorage a
fromList = CIStorage . H.fromList . map (first CI.mk)

lookup :: Text -> CIStorage a -> Maybe a
lookup key ciStorage = H.lookup (CI.mk key) $ unCIStorage ciStorage

----
postfixFields :: LensRules
postfixFields = lensRules & lensField .~ mappingNamer (\n -> [n ++ "L"])

whenT :: (Applicative m) => Bool -> m Bool -> m Bool
whenT cond_ action_ = if cond_ then action_ else pure False
