-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Util where

import Universum

import Data.Aeson (FromJSON(..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Casing
import Data.Yaml qualified as Y
import Language.Haskell.TH

attach :: (Functor f) => (a -> b) -> f a -> f (a, b)
attach f = fmap (\x -> (x, f x))

-- | Options that we use to derive JSON instances for config types.
aesonConfigOption :: Aeson.Options
aesonConfigOption = (aesonPrefix camelCase){Aeson.rejectUnknownFields = True}

-- | Usage: $(fst trick) $ snd trick, where (trick :: Trick a) is defined in another module.
type Trick a = (Q Exp, a)

thTrickYaml :: forall a. (FromJSON a) => ByteString -> Trick a
thTrickYaml input = case first show $ Y.decodeEither' input of
  Left str -> (fail str, error "")
  Right c -> ([|id|], c)
