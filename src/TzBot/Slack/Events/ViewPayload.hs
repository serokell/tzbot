-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

{- | This module contains some convenient machinery to easily extract
 - the state of the modal elements (like input blocks or switches).
 - To use it, just wrap your datatype with `ViewPayload` newtype
 - and provide correct `blockId` and `actionId` type level values,
 - then construct a record of these `ViewPayloads` wrapped datatypes
 - and derive `FromJSON` instance via `PayloadCollection` newtype.
 - This `FromJSON` instance will lookup .values.(blockId).(actionId).value
 - path of the provided aeson `Value`.
 -
 - See for details:
 - https://api.slack.com/reference/interaction-payloads/views
 -}
module TzBot.Slack.Events.ViewPayload
  ( PayloadCollection (..)
  , ViewPayload (..)
  ) where

import Universum

import Data.Aeson (FromJSON(parseJSON), Value)
import Data.Aeson.Lens (key)
import Data.Aeson.Types (Parser)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Text.Interpolation.Nyan (int, rmode')

import TzBot.Slack.API (ActionId(..), BlockId(..))

-- | Newtype for DerivingVia
newtype PayloadCollection a =
  PayloadCollection { unPayloadCollection :: a }

instance (Generic a, GFromViewPayload (Rep a))
    => FromJSON (PayloadCollection a) where
  parseJSON val = PayloadCollection <$> fromViewPayload val

fromViewPayload
  :: (Generic a, GFromViewPayload (Rep a))
  => Value
  -> Parser a
fromViewPayload val = to <$> gFromViewPayload val

class GFromViewPayload f where
  gFromViewPayload :: Value -> Parser (f a)

-- We ignore all the meta information
instance GFromViewPayload f => GFromViewPayload (M1 a d f) where
  gFromViewPayload val = M1 <$> gFromViewPayload val

instance (GFromViewPayload f, GFromViewPayload g) => GFromViewPayload (f :*: g) where
  gFromViewPayload val = (:*:) <$> gFromViewPayload val <*> gFromViewPayload val

instance (FromJSON a, KnownSymbol blockId, KnownSymbol actionId)
    => GFromViewPayload (Rec0 $ ViewPayload blockId actionId a) where
  gFromViewPayload val = K1 <$> parseJSON val

-- | A single value of a single block/action pair of the view.
-- FromJSON instance will try to lookup the .values.(blockId).(actionId).value
-- path of the given JSON value.
-- See https://api.slack.com/reference/interaction-payloads/views
newtype ViewPayload (blockId :: Symbol) (actionId :: Symbol) a
  = ViewPayload { unViewPayload :: a }
  deriving stock (Show)

instance (FromJSON a, KnownSymbol blockIdT, KnownSymbol actionIdT)
    => FromJSON (ViewPayload blockIdT actionIdT a) where
  parseJSON val = do
    let blockId = fromString $ symbolVal $ Proxy @blockIdT
    let actionId = fromString $ symbolVal $ Proxy @actionIdT
    let pathError = [int||Path .values.#{blockId}.#{actionId}.value not found.|]
    item <- maybe (fail pathError) pure $
      getViewPayloadItem blockId actionId val
    ViewPayload <$> parseJSON item

getViewPayloadItem :: BlockId -> ActionId -> Value -> Maybe Value
getViewPayloadItem blockId actionId val =
  val ^? key "values"
    . key (unBlockId blockId)
    . key (unActionId actionId)
    . key "value"
