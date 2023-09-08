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
  (FromJSON(..), Options(..), ToJSON(toJSON), Value, camelTo2, genericParseJSON, genericToJSON)
import Data.Aeson.Lens (_String, key)
import Data.String.Conversions (cs)
import Data.Text.Internal.Builder (Builder, fromText, toLazyText)
import Deriving.Aeson (CamelToSnake, ConstructorTagModifier, CustomJSON(..), StripPrefix)

import TzBot.Util

newtype MessageBlock = MessageBlock
  { mbElements :: [BlockElementLevel1]
  } deriving stock (Eq, Show, Generic)
    deriving (FromJSON) via RecordWrapper MessageBlock

data BlockElementLevel1
  = BEL1List RichTextList
  | BEL1Plain PlainBlockElementLevel1
    deriving stock (Eq, Show, Generic)
    deriving (FromJSON) via SumWrapper BlockElementLevel1

data RichTextList = RichTextList
  { rtlElements :: [BlockElementLevel1]
  } deriving stock (Eq, Show, Generic)
    deriving (FromJSON) via TypedWrapper RichTextList

data BlockElementType
  = BETRichTextSection -- ^ Simple text section
  | BETRichTextPreformatted -- ^ Multiline code block
  | BETRichTextQuote -- ^ Slack quote
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[StripPrefix "BET", CamelToSnake]] BlockElementType

data PlainBlockElementLevel1 = PlainBlockElementLevel1
  { beType     :: WithUnknown BlockElementType
  , beElements :: Maybe [WithUnknown BlockElementLevel2]
    -- ^ Level 2 elements
  } deriving stock (Eq, Show, Generic)
    deriving (FromJSON) via RecordWrapper PlainBlockElementLevel1

----------------------------------------------------------------------------
-- Level 2 elements
----------------------------------------------------------------------------

data BlockElementLevel2
  = BEL2ElementLink ElementLink
  | BEL2ElementText ElementText
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via SumWrapper BlockElementLevel2

-- | A json object will be parsed to a record type @ElementX@
-- only if it has a field @"type": "x"@.
blockElementOptions :: Options
blockElementOptions = defaultTypedOptions
  { constructorTagModifier = camelTo2 '_' . stripPrefixIfPresent "Element"
  }

{- | Example:
@
{
    "type": "link",
    "url": "https://issues.serokell.io/issue/LIGO-205",
    "text": "Issue"
}
@
-}
data ElementLink = ElementLink
  { elText :: Text
  , etUrl :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ElementLink where parseJSON = genericParseJSON blockElementOptions
instance ToJSON ElementLink where toJSON = genericToJSON blockElementOptions

{- | Examples:
@
{
  "type": "text",
  "text": "some text",
  "style": {
    "bold": true
  }
}
@
@
{
  "type": "text",
  "text": "x :: Int",
  "style": {
    "code": true
  }
}
@
-}
data ElementText = ElementText
  { etText  :: Text
  , etStyle :: Maybe Style
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON ElementText where parseJSON = genericParseJSON blockElementOptions
instance ToJSON ElementText where toJSON = genericToJSON blockElementOptions

data Style = Style
  { styCode   :: Maybe Bool
  , styStrike :: Maybe Bool
  , styItalic :: Maybe Bool
  , styBold   :: Maybe Bool
  } deriving stock (Eq, Show, Generic)
    deriving (FromJSON, ToJSON) via RecordWrapper Style

----------------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------------

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
--
-- Notes:
--   * If it finds two adjacent "text"/"link" blocks, it will join their contents into a single string.
--   * If it finds an inline code block in the middle of a sentence,
--     it'll ignore the code and break the string in two,
--     e.g. The message @Some `inline code` here@ will be converted to @["Some", "here"]@
--   * If it finds an unrecognized level 2 block in the middle of a sentence,
--     it'll ignore the block and break the string in two.
extractPieces :: [MessageBlock] -> ([Text], [ExtractError])
extractPieces mBlocks = runWriter $ concat <$> mapM goMessageBlock mBlocks
  where
  goMessageBlock :: MessageBlock -> Writer [ExtractError] [Text]
  goMessageBlock msgBlock = concat <$> mapM goBlockElementLevel1 msgBlock.mbElements

  goBlockElementLevel1 :: BlockElementLevel1 -> Writer [ExtractError] [Text]
  goBlockElementLevel1 = \case
    BEL1List RichTextList {rtlElements} -> concat <$> mapM goBlockElementLevel1 rtlElements
    BEL1Plain PlainBlockElementLevel1 {beType, beElements} -> do
      whenLeft (unUnknown beType) \val ->
        tell [EEUnknownBlockElementLevel1Type $ UnknownBlockElementLevel1Type val]
      -- ignore multiline code block
      case beType of
        WithUnknown (Right BETRichTextPreformatted) -> pure []
        _ -> maybe (pure []) goBlockElementLevel2 beElements

  goBlockElementLevel2 :: [WithUnknown BlockElementLevel2] -> Writer [ExtractError] [Text]
  goBlockElementLevel2 els = reverse <$> go Nothing [] els
    where
    go :: Maybe Builder -> [Text] -> [WithUnknown BlockElementLevel2] -> Writer [ExtractError] [Text]
    go mbCurPiece prevPieces (e:es) = case unUnknown e of
      Left val -> do
        let blockType = fromMaybe "unknown" (val ^? key "type" . _String)
        case blockType of
          "emoji" ->
            -- skip over emoji blocks
            go mbCurPiece prevPieces es
          _ -> do
            tell [EEUnknownBlockElementLevel2 $ UnknownBlockElementLevel2Error blockType val]
            -- break the message in two separate `Text` pieces.
            go Nothing (prependMbCurrentToPrevious mbCurPiece prevPieces) es
      Right (BEL2ElementText elementText) -> do
        let etTextB = fromText elementText.etText
        if (elementText.etStyle >>= styCode) == Just True
          then
            -- ignore inline code block
            go Nothing (prependMbCurrentToPrevious mbCurPiece prevPieces) es
          else
            -- collate this block's text with any adjacent text-like block
            go (Just $ maybe etTextB (<> etTextB) mbCurPiece) prevPieces es
      Right (BEL2ElementLink elementLink) -> do
        let linkText = fromText elementLink.elText
        go (Just $ maybe linkText (<> linkText) mbCurPiece) prevPieces es
    go mbCurPiece prevPieces [] =
      pure $ prependMbCurrentToPrevious mbCurPiece prevPieces

    prependMbCurrentToPrevious :: Maybe Builder -> [Text] -> [Text]
    prependMbCurrentToPrevious mbCurPiece prevPieces =
      maybe prevPieces ((: prevPieces) . cs . toLazyText) mbCurPiece
