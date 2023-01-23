-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.ProcessEvents.Common
  ( openModalCommon
  , getTimeReferencesFromMessage

    -- * exported for tests
  , ignoreCodeBlocksManually
  ) where

import Universum

import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Data.GUID (genText)
import Data.Text qualified as T
import Fmt (listF)
import Text.Interpolation.Nyan (int, rmode')

import TzBot.Feedback.Dialog (insertDialogEntry)
import TzBot.Feedback.Dialog.Types
import TzBot.Parser (parseTimeRefs)
import TzBot.Render (TranslationPairs, renderAllForOthersTP, renderTemplate)
import TzBot.RunMonad (log')
import TzBot.Slack (BotM, getUserCached, startModal)
import TzBot.Slack.API
import TzBot.Slack.API.MessageBlock
  (UnknownBlockElementLevel2Error(ubeType), extractPieces, splitExtractErrors)
import TzBot.TimeReference (TimeReference)
import TzBot.Util (WithUnknown(unUnknown))

-- | Generic function that starts view or report modal where
--   time references translations can be viewed
--   and feedback can be left.
openModalCommon
  :: Message
  -> ChannelId
  -> UserId
  -> TriggerId
  -> (Text -> Maybe TranslationPairs -> ReportDialogId -> Modal)
  -- ^ The way how to build a modal.
  -- See "TzBot.Slack.Modal" for possible implementations
  -> BotM ()
openModalCommon message channelId whoTriggeredId triggerId mkModalFunc = do
  let msgText = mText message
      msgTimestamp = mTs message
  mbTimeRefs <- nonEmpty <$> getTimeReferencesFromMessage message
  sender <- getUserCached $ mUser message
  translationPairs <- forM mbTimeRefs $ \neTimeRefs -> do
      whoTriggered <- getUserCached whoTriggeredId
      pure $
        renderAllForOthersTP whoTriggered $
          renderTemplate msgTimestamp sender neTimeRefs

  guid <- ReportDialogId <$> liftIO genText
  let metadata = ReportDialogEntry
        { rpmMessageText = mText message
        , rpmTimeTranslation = translationPairs
        , rpmSenderTimeZone = uTz sender
        , rpmMessageTimestamp = mTs message
        , rpmUserId = whoTriggeredId
        , rpmChannelId = channelId
        , rpmThreadId = mThreadId message
        }
  insertDialogEntry guid metadata
  let modal = mkModalFunc msgText translationPairs guid
  startModal $ OpenViewReq modal triggerId

-- | Extract separate text pieces from the Slack message that can contain
-- the whole time reference and try to find time references inside them.
getTimeReferencesFromMessage
  :: Message
  -> BotM [TimeReference]
getTimeReferencesFromMessage message =
  concatMap parseTimeRefs <$> getTextPiecesFromMessage message

-- | Extract separate text pieces from the Slack message that can contain
-- the whole time reference. The main way is analyzing the message's block
-- structure, but since it's not documented it can not work sometimes,
-- `ignoreCodeBlocksManually` is used a reserve function.
getTextPiecesFromMessage
  :: Message
  -> BotM [Text]
getTextPiecesFromMessage message = do
  let throwAwayTooShort = filter (\x -> T.compareLength x 2 == GT)
  throwAwayTooShort <$> case unUnknown $ msgBlocks message of
    Left unknownBlocksValue -> do
      log' [int||warning: Failed to parse message blocks, \
                 trying to ignore code blocks manually|]
      log' [int||warning: Unrecognized message blocks: \
                 #{encodePrettyToTextBuilder unknownBlocksValue}|]
      pure $ ignoreCodeBlocksManually $ mText message
    Right blocks -> do
      let (pieces, exErrs) = extractPieces blocks
      let (l1Errs, l2Errs) = splitExtractErrors exErrs
      when (not $ null l1Errs) $
        log' [int||warning: Unknown level1 block types: #{listF $ map (show @Text) l1Errs}|]
      when (not $ null l2Errs) $
        log' [int||warning: Unknown level2 block types: #{listF $ map ubeType l2Errs}|]
      pure pieces

{- | Simple function that works in a very naive way, but it's just reserve
 - option when Slack blocks can't be parsed.
 -}
ignoreCodeBlocksManually :: Text -> [Text]
ignoreCodeBlocksManually txt =
  concatMap (splitByCodeBlocks simpleCodeDelimiter) $
    splitByCodeBlocks codeBlockDelimiter txt
  where
  codeBlockDelimiter = "```"
  simpleCodeDelimiter = "`"

  splitByCodeBlocks :: Text -> Text -> [Text]
  splitByCodeBlocks _ "" = []
  splitByCodeBlocks delim txt = do
    let (xs, ys) = T.breakOn delim txt
    case T.stripPrefix delim ys of
      Nothing -> [xs]
      Just ys' -> do
        let (_ignored, zs) = T.breakOn delim ys'
        case T.stripPrefix delim zs of
          Nothing -> [txt]
          Just zs' -> xs : splitByCodeBlocks delim zs'
