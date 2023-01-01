-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Util (
  module TzBot.Util,
  ) where

import Universum hiding (last)

import Data.Aeson
import Data.Aeson qualified as AeKey
import Data.Aeson qualified as Aeson
import Data.Aeson.Casing
import Data.Aeson.Key qualified as AeKey
import Data.Aeson.Types
import Data.Char
import Data.String.Conversions
import Data.Time
import Data.Yaml qualified as Y
import GHC.Generics
import Language.Haskell.TH
import Text.Interpolation.Nyan

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
aesonConfigOption :: Aeson.Options
aesonConfigOption = (aesonPrefix camelCase){Aeson.rejectUnknownFields = True}

-- | Usage: $(fst trick) $ snd trick, where (trick :: Trick a) is defined in another module.
type Trick a = (Q Exp, a)

thTrickYaml :: forall a. (FromJSON a) => ByteString -> Trick a
thTrickYaml input = case first show $ Y.decodeEither' input of
  Left str -> (fail str, error "")
  Right c -> ([|id|], c)

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

----
defaultSumOptions :: Options
defaultSumOptions = defaultOptions
  { sumEncoding = UntaggedValue
  }

newtype SumWrapper a = SumWrapper a

instance (Generic a, GToJSON' Value Zero (Rep a)) => ToJSON (SumWrapper a) where
  toJSON (SumWrapper x) = genericToJSON defaultSumOptions x

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
