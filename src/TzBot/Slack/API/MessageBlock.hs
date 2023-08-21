-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

{- | This module contains datatypes that are used to parse the message blocks
 - that the message objects are shipped with. They seem to be not properly
 - documented and we have to be safe and assume that some unknown objects
 - can appear.
 - See:
 - * https://api.slack.com/changelog/2019-09-what-they-see-is-what-you-get-and-more-and-less
 - * https://api.slack.com/reference/messaging/payload
 -}
module TzBot.Slack.API.MessageBlock
  ( -- * Block datatype
    MessageBlock

    -- * Extract errors (or, more precisely, warnings)
  , ExtractError (..)
  , UnknownBlockElementLevel1Type (..)
  , UnknownBlockElementLevel2Error (..)

    -- * Functions
  , extractPieces
  , splitExtractErrors
  ) where

import TzPrelude

import Control.Lens
import Control.Monad.Trans.Writer.CPS (Writer, runWriter, tell)
import Data.Aeson
  (FromJSON(..), Options(..), SumEncoding(..), ToJSON(toJSON), Value, camelTo2, defaultOptions,
  genericParseJSON, genericToJSON)
import Data.Aeson.Lens (_String, key)
import Data.Char (isLower)
import Data.String.Conversions (cs)
import Data.Text.Internal.Builder (Builder, fromText, toLazyText)
import Deriving.Aeson (CamelToSnake, ConstructorTagModifier, CustomJSON(..), StripPrefix)

import TzBot.Util

newtype MessageBlock = MessageBlock
  { mbElements :: [BlockElementLevel1]
  } deriving stock (Eq, Show, Generic)
    deriving (FromJSON, ToJSON) via RecordWrapper MessageBlock

data BlockElementLevel1
  = BEL1List RichTextList
  | BEL1Plain PlainBlockElementLevel1
    deriving stock (Eq, Show, Generic)
    deriving (FromJSON, ToJSON) via SumWrapper BlockElementLevel1

data RichTextList = RichTextList
  { rtlElements :: [BlockElementLevel1]
  } deriving stock (Eq, Show, Generic)
    deriving (FromJSON, ToJSON) via TypedWrapper RichTextList

data BlockElementType
  = BETRichTextSection -- ^ Simple text section
  | BETRichTextPreformatted -- ^ Multiline code block
  | BETRichTextQuote -- ^ Slack quote
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[StripPrefix "BET", CamelToSnake]] BlockElementType

data PlainBlockElementLevel1 = PlainBlockElementLevel1
  { beType     :: WithUnknown BlockElementType
  , beElements :: Maybe [WithUnknown ElementText]
    -- ^ Level 2 elements
  } deriving stock (Eq, Show, Generic)
    deriving (FromJSON, ToJSON) via RecordWrapper PlainBlockElementLevel1

----
data Style = Style
  { styCode   :: Maybe Bool
  , styStrike :: Maybe Bool
  , styItalic :: Maybe Bool
  , styBold   :: Maybe Bool
  } deriving stock (Eq, Show, Generic)
    deriving (FromJSON, ToJSON) via RecordWrapper Style

--
-- | Here it's the only level 2 element because we are not interested
-- in others at the current moment.
data ElementText = ElementText
  { etText  :: Text
  , etStyle :: Maybe Style
  } deriving stock (Eq, Show, Generic)

blockElementOptions :: Options
blockElementOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . dropWhile isLower
  , constructorTagModifier = camelTo2 '_' . stripPrefixIfPresent "Element"
  , tagSingleConstructors = True
  , sumEncoding = TaggedObject "type" "contents"
  , omitNothingFields = True
  }

instance FromJSON ElementText where
  parseJSON = genericParseJSON blockElementOptions

instance ToJSON ElementText where
  toJSON = genericToJSON blockElementOptions

----
data ExtractError
  = EEUnknownBlockElementLevel1Type UnknownBlockElementLevel1Type
  | EEUnknownBlockElementLevel2 UnknownBlockElementLevel2Error
  deriving stock (Eq, Show)

splitExtractErrors
  :: [ExtractError]
  -> ([UnknownBlockElementLevel1Type], [UnknownBlockElementLevel2Error])
splitExtractErrors = partitionEithers . map f
  where
  f (EEUnknownBlockElementLevel1Type val) = Left val
  f (EEUnknownBlockElementLevel2 l2Val) = Right l2Val

newtype UnknownBlockElementLevel1Type = UnknownBlockElementLevel1Type
  { ubeltValue :: Value
  } deriving stock (Eq, Show)

data UnknownBlockElementLevel2Error = UnknownBlockElementLevel2Error
  { ubeType  :: Text
  , ubeValue :: Value
  } deriving stock (Eq, Show)

-- | This function has two main tasks:
-- 1. Analyze the Slack-provided structure of the incoming message;
-- 2. Ignore code blocks.
--
-- Also since the message blocks are not documented, it collects unrecognized
-- values of level1/level2 block elements.
extractPieces :: [MessageBlock] -> ([Text], [ExtractError])
extractPieces mBlocks = runWriter $ concat <$> mapM goMessageBlock mBlocks
  where
  goMessageBlock :: MessageBlock -> Writer [ExtractError] [Text]
  goMessageBlock MessageBlock {..} = concat <$> mapM goBlockElementLevel1 mbElements

  goBlockElementLevel1 :: BlockElementLevel1 -> Writer [ExtractError] [Text]
  goBlockElementLevel1 = \case
    BEL1List RichTextList {..} -> concat <$> mapM goBlockElementLevel1 rtlElements
    BEL1Plain PlainBlockElementLevel1 {..} -> do
      whenLeft (unUnknown beType) \val ->
        tell [EEUnknownBlockElementLevel1Type $ UnknownBlockElementLevel1Type val]
      -- ignore multiline code block
      case beType of
        WithUnknown (Right BETRichTextPreformatted) -> pure []
        _ -> maybe (pure []) goBlockElementLevel2 beElements

  goBlockElementLevel2 :: [WithUnknown ElementText] -> Writer [ExtractError] [Text]
  goBlockElementLevel2 els = reverse <$> go Nothing [] els
    where
    go :: Maybe Builder -> [Text] -> [WithUnknown ElementText] -> Writer [ExtractError] [Text]
    go mbCurPiece prevPieces (e:es) = case unUnknown e of
      Left val -> do
        let _type = fromMaybe "unknown" (val ^? key "type" . _String)
        tell [EEUnknownBlockElementLevel2 $ UnknownBlockElementLevel2Error _type val]
        go Nothing (prependMbCurrentToPrevious mbCurPiece prevPieces) es
      Right ElementText {..} -> do
        let etTextB = fromText etText
        if (etStyle >>= styCode) == Just True
          -- ignore simple code block
          then go Nothing (prependMbCurrentToPrevious mbCurPiece prevPieces) es
          else go (Just $ maybe etTextB (<> etTextB) mbCurPiece) prevPieces es
    go mbCurPiece prevPieces [] =
      pure $ prependMbCurrentToPrevious mbCurPiece prevPieces

    prependMbCurrentToPrevious :: Maybe Builder -> [Text] -> [Text]
    prependMbCurrentToPrevious mbCurPiece prevPieces =
      maybe prevPieces ((: prevPieces) . cs . toLazyText) mbCurPiece
