-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Render
  ( -- * Types
    TranslationPair
  , TranslationPairs
  , Template

    -- * Render generic
  , renderAllTP

    -- * Render text
  , renderAll
  , joinTranslationPairs

    -- * Render Slack
  , renderSlackBlocks

    -- * General template
  , renderTemplate

    -- * Flags
  , SenderFlag (..)
  , asForSenderS
  , asForOthersS

  , ModalFlag (..)
  , asForModalM
  , asForMessageM
  ) where

import Universum

import Data.Aeson (ToJSON)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder, fromText, singleton, toLazyText)
import Data.Time.Compat
  (LocalTime(localDay), UTCTime, defaultTimeLocale, formatTime, pattern YearMonthDay)
import Data.Time.TZInfo qualified as TZI
import Data.Time.TZTime qualified as TZT
import Data.Time.Zones.All (TZLabel)
import Text.Interpolation.Nyan (int, rmode')

import TzBot.Instances ()
import TzBot.Slack.API (Mrkdwn(Mrkdwn), PlainText(..), User(..))
import TzBot.Slack.API.Block
import TzBot.TimeReference
import TzBot.Util

-- Types

-- We use `Left` to keep translation errors (invalid/ambiguous time,
-- invalid offset abbreviation) and they are common for all users,
-- and valid translations (`Right`) depend on the receiver timezone.
type EitherTemplateUnit = Either TranslationPair (User -> Maybe TranslationPair)

data TranslationPair = TranslationPair
  { tuTimeRef :: Text
    -- ^ The piece of the original message containing a time reference
  , tuTranslation :: Text
    -- ^ Valid or invalid translation of the time reference
  , tuNoteForSender :: Maybe Text
    -- ^ Additional optional information for the sender
  , tuNoteForOthers :: Maybe Text
    -- ^ Additional optional information for others (not the sender)
  } deriving stock (Eq, Show, Generic)
    deriving ToJSON via RecordWrapper TranslationPair

type TranslationPairs = NE.NonEmpty TranslationPair

newtype Template = Template { unTemplate :: NE.NonEmpty EitherTemplateUnit }

---------------------------------------
-- Notes
---------------------------------------
-- | Used for choosing between two notes
newtype SenderFlag = SenderFlag { unSenderFlag :: Bool }

asForSenderS, asForOthersS :: SenderFlag
asForSenderS = SenderFlag True
asForOthersS = SenderFlag False

chooseNote :: SenderFlag -> TranslationPair -> Maybe Text
chooseNote (SenderFlag sender) = if sender then tuNoteForSender else tuNoteForOthers

--
concatTranslationPair :: SenderFlag -> TranslationPair -> Builder
concatTranslationPair sender t@TranslationPair {..} = do
  let rightNote = chooseNote sender t
  let note = maybe "" (("\n" <>) . fromText) rightNote
  [int||#{tuTimeRef}:  #{tuTranslation}#{note}|]

renderAllTP :: User -> Template -> Maybe TranslationPairs
renderAllTP user =
  nonEmpty . mapMaybe (either Just ($ user)) . NE.toList . unTemplate

-- | We show this message because we may have not found
-- any time references while actually there were.
noRefsFoundMsg :: Text
noRefsFoundMsg = "No time references found."

-- Render text
renderAll :: User -> Template -> Maybe Text
renderAll user =
  fmap (joinTranslationPairs asForSenderS) . renderAllTP user

joinTranslationPairs :: SenderFlag -> TranslationPairs -> Text
joinTranslationPairs sender =
  TL.toStrict . toLazyText . fold . NE.toList
    . NE.map ((<> singleton '\n') . concatTranslationPair sender)

-- Render Slack block
renderSlackBlocks :: SenderFlag -> Maybe TranslationPairs -> [Block]
renderSlackBlocks forSender =
  maybe [noRefsFoundSection]
    (intercalate [BDivider divider] . NE.toList . NE.map mkTranslationBlocks)
  where
    noRefsFoundSection = BSection $ textSection (PlainText noRefsFoundMsg) Nothing
    mkTranslationBlocks :: TranslationPair -> [Block]
    mkTranslationBlocks timeRef = do
      let t = (Mrkdwn $ tuTimeRef timeRef, Mrkdwn $ tuTranslation timeRef)
          mbNote = chooseNote forSender timeRef
          translationBlock = BSection $ fieldsSection Nothing Nothing $ NE.singleton t
          mkNoteBlock note = BSection $ markdownSection (Mrkdwn note) Nothing
      withMaybe mbNote [translationBlock] $ \note -> [translationBlock, mkNoteBlock note]

renderTemplate :: ModalFlag -> UTCTime -> User -> NE.NonEmpty TimeReference -> Template
renderTemplate asForModal now sender timeRefs =
  Template $ NE.map (renderEphemeralMessageTranslationPair asForModal now sender)
    $ attach (timeReferenceToUTC (uTz sender) now) timeRefs

--
newtype ModalFlag = ModalFlag { unModalFlag :: Bool }

asForModalM, asForMessageM :: ModalFlag
asForModalM = ModalFlag True
asForMessageM = ModalFlag False

--
renderTranslationOnSuccess
  :: ModalFlag
  -> UTCTime
  -> User
  -> TimeReference
  -> TimeRefSuccess
  -> User
  -> Maybe TranslationPair
renderTranslationOnSuccess
  (ModalFlag forModal) now sender timeRef (TimeRefSuccess {..}) user
    = do
  let userTzLabel = uTz user
      isSender = ((/=) `on` uId) sender user
      shouldShowThisTranslation = isSender || forModal
      renderedUserTime =
        T.pack $ renderUserTime trsIsDateInferred userTzLabel now trsUtcResult
      mbRenderedTranslation = case trsEithTzOffset of
        Right _ -> Just renderedUserTime
        Left refTzLabel ->
          if refTzLabel == userTzLabel
          then guard shouldShowThisTranslation >> Just "*your timezone is the same*"
          else Just renderedUserTime

  (<$> mbRenderedTranslation) $ \rt -> TranslationPair
      (renderOriginalTimeRef sender timeRef)
      rt
      Nothing
      Nothing

renderOriginalTimeRef :: User -> TimeReference -> Text
renderOriginalTimeRef sender timeRef = do
  let mbSenderTimeZone :: Maybe Builder
      mbSenderTimeZone = case trLocationRef timeRef of
        Just _ -> Nothing
        Nothing -> Just [int|| in #{uTz sender}|]
  [int||"#{trText timeRef}"#{mbSenderTimeZone}:|]

-- Under `Left` we collect errors, that should be shown to all users (including sender),
-- and under `Right` we collect valid time references that should be rendered differently
-- for each user.
renderEphemeralMessageTranslationPair
  :: ModalFlag
  -> UTCTime
  -> User
  -> (TimeReference, TimeReferenceToUTCResult)
  -> EitherTemplateUnit
renderEphemeralMessageTranslationPair asForModal now sender (timeRef, result)
    = case result of
  TRTUSuccess trSucc ->
    Right $ renderTranslationOnSuccess asForModal now sender timeRef trSucc
  TRTUAmbiguous timeShiftErrorInfo -> do
    let shownTZ = shownTimezone timeShiftErrorInfo
    Left $ TranslationPair
      { tuTimeRef = renderOriginalTimeRef sender timeRef
      , tuTranslation = "*ambiguous because of the time shift*"
      , tuNoteForSender = Just
          [int|n|_There is a timeshift in #{shownTZ True} around the specified
                  time and this particular timestamp can be possible with
                  different offsets, please define the offset explicitly._
          |]
      , tuNoteForOthers = Just
          [int|n|_There is a time zone shift in #{shownTZ False} around that
                  time, and this particular timestamp can be possible with
                  different offsets._
          |]
      }
  TRTUInvalid timeShiftErrorInfo -> do
    let shownTZ = shownTimezone timeShiftErrorInfo
    Left $ TranslationPair
      { tuTimeRef = renderOriginalTimeRef sender timeRef
      , tuTranslation = "*invalid because of the time shift*"
      , tuNoteForSender = Just
          [int|n|_There is a timeshift in #{shownTZ True} around the specified
                  time and this particular timestamp does not exist,
                  please define the offset explicitly._
          |]
      , tuNoteForOthers = Just
          [int|n|_There is a time zone shift in #{shownTZ False} around that
                  time, and this particular timestamp does not exist._
          |]
      }
  TRTUInvalidTimeZoneAbbrev abbrev ->
    Left $ TranslationPair
      (trText timeRef)
      [int||contains invalid timezone abbreviation: #{abbrev}|]
      Nothing -- TODO: We can find some abbreviations that are similar to
              -- what the sender defined and that we know about
      Nothing
  where
    shownTimezone :: TimeShiftErrorInfo -> Bool -> Builder
    shownTimezone (TimeShiftErrorInfo {..}) forSender
      | tseiIsImplicitSenderTimezone =
          if forSender
          then [int||your timezone (#{tseiRefTimeZone})|]
          else [int||the sender's timezone (#{tseiRefTimeZone})|]
      | otherwise = [int||#{tseiRefTimeZone}|]

renderUserTime :: Bool -> TZLabel -> UTCTime -> UTCTime -> String
renderUserTime isDateInferred tzLabel now refTime = do
  let tzInfo = TZI.fromLabel tzLabel
      currentUserLocalTime = TZT.tzTimeLocalTime $ TZT.fromUTC tzInfo now
      refUserLocalTime = TZT.tzTimeLocalTime $ TZT.fromUTC tzInfo refTime
      sameYear = theYearIsTheSame currentUserLocalTime refUserLocalTime
      yearFormat = if sameYear then "" else ", %Y" :: Builder
      dateFormat = [int||%A, %B %d|] :: Builder
      timeFormat = [int||%H:%M in #{tzLabel}|] :: Builder
      format = if not isDateInferred
               then [int||*#{timeFormat}, #{dateFormat}#{yearFormat}*|] :: String
               else [int||*#{timeFormat}* (possibly, #{dateFormat}#{yearFormat})|]

  formatTime defaultTimeLocale format refUserLocalTime

theYearIsTheSame :: LocalTime -> LocalTime -> Bool
theYearIsTheSame t1 t2 = do
  let YearMonthDay y1 _ _ = localDay t1
  let YearMonthDay y2 _ _ = localDay t2
  y1 == y2
