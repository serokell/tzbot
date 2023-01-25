-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Render
  ( -- * Types
    TranslationPair
  , TranslationPairs
  , Template

    -- * Render generic
  , renderAllForOthersTP
  , renderErrorsForSenderTP

    -- * Render text
  , renderErrorsForSender
  , renderAllForOthers
  , joinTranslationPairs

    -- * Render Slack
  , renderSlackBlocks

    -- * General template
  , renderTemplate
  ) where

import Universum

import Data.Aeson (ToJSON)
import Data.List.NonEmpty qualified as NE
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Builder (Builder, fromText, singleton, toLazyText)
import Data.Time.Compat
  (LocalTime(localDay), UTCTime, defaultTimeLocale, formatTime, pattern YearMonthDay)
import Data.Time.TZInfo qualified as TZI
import Data.Time.TZTime qualified as TZT
import Data.Time.Zones.All (TZLabel)
import Text.Interpolation.Nyan (int, rmode')

import TzBot.Instances ()
import TzBot.Slack.API (Mrkdwn(Mrkdwn), PlainText(..), User(uTz))
import TzBot.Slack.API.Block
import TzBot.TimeReference
import TzBot.Util

-- Types

-- We use `Left` to keep translation errors (invalid/ambiguous time,
-- invalid offset abbreviation) and they are common for all users,
-- and valid translations (`Right`) depend on the receiver timezone.
type EitherTemplateUnit = Either TranslationPair (User -> TranslationPair)

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

chooseNote :: Bool -> TranslationPair -> Maybe Text
chooseNote sender = if sender then tuNoteForSender else tuNoteForOthers

concatTranslationPair :: Bool -> TranslationPair -> Builder
concatTranslationPair sender t@TranslationPair {..} = do
  let rightNote = chooseNote sender t
  let note = maybe "" (("\n" <>) . fromText) rightNote
  [int||#{tuTimeRef}:  #{tuTranslation}#{note}|]

-- Render generic
renderErrorsForSenderTP :: Template -> Maybe TranslationPairs
renderErrorsForSenderTP (Template lst) = NE.nonEmpty $ lefts $ NE.toList lst

renderAllForOthersTP :: User -> Template -> TranslationPairs
renderAllForOthersTP user = NE.map (either id ($ user)) . unTemplate

-- | We show this message because we may have not found
-- any time references while actually there were.
noRefsFoundMsg :: Text
noRefsFoundMsg = "No time references found."

-- Render text
renderErrorsForSender :: Template -> Maybe Text
renderErrorsForSender template = do
  let sender = True
  joinTranslationPairs sender <$> renderErrorsForSenderTP template

renderAllForOthers :: User -> Template -> Text
renderAllForOthers user = do
  let sender = False
  joinTranslationPairs sender . renderAllForOthersTP user

joinTranslationPairs :: Bool -> TranslationPairs -> Text
joinTranslationPairs sender =
  T.toStrict . toLazyText . fold . NE.toList
    . NE.map ((<> singleton '\n') . concatTranslationPair sender)

-- Render Slack block
renderSlackBlocks :: Bool -> Maybe TranslationPairs -> [Block]
renderSlackBlocks forSender =
  maybe [noRefsFoundSection]
    (intercalate [BDivider divider] . NE.toList . NE.map mkTranslationBlocks)
  where
    noRefsFoundSection = BSection $ textSection (PlainText noRefsFoundMsg) Nothing
    mkTranslationBlocks :: TranslationPair -> [Block]
    mkTranslationBlocks timeRef = do
      let t = (PlainText $ tuTimeRef timeRef, PlainText $ tuTranslation timeRef)
          mbNote = chooseNote forSender timeRef
          translationBlock = BSection $ fieldsSection Nothing Nothing $ NE.singleton t
          mkNoteBlock note = BSection $ markdownSection (Mrkdwn note) Nothing
      withMaybe mbNote [translationBlock] $ \note -> [translationBlock, mkNoteBlock note]

renderTemplate :: UTCTime -> User -> NE.NonEmpty TimeReference -> Template
renderTemplate now sender timeRefs =
  Template $ NE.map (renderEphemeralMessageTranslationPair now sender)
    $ attach (timeReferenceToUTC (uTz sender) now) timeRefs

-- Under `Left` we collect errors, that should be shown to all users (including sender),
-- and under `Right` we collect valid time references that should be rendered differently
-- for each user.
renderEphemeralMessageTranslationPair
  :: UTCTime
  -> User
  -> (TimeReference, TimeReferenceToUTCResult)
  -> EitherTemplateUnit
renderEphemeralMessageTranslationPair now sender (timeRef, result) = case result of
  TRTUSuccess utcTime _offsetInfo -> do
    let mbSenderTimeZone =
          guard (isNothing $ trLocationRef timeRef)
            $> (uTz sender) :: Maybe TZLabel
        mbSenderTimeZoneAux = mbSenderTimeZone $> " in " :: Maybe Text
    Right $ \user -> do
      let userTzLabel = uTz user
          renderedUserTime = renderUserTime userTzLabel now utcTime
      TranslationPair
        [int||"#{trText timeRef}"#{mbSenderTimeZoneAux}#{mbSenderTimeZone}|]
        [int||#{renderedUserTime} in #{userTzLabel}|]
        Nothing
        Nothing
  TRTUAmbiguous implicitSenderTimezone tzLabel -> do
    let shownTZ = shownTimezone implicitSenderTimezone tzLabel
    Left $ TranslationPair
      { tuTimeRef = trText timeRef
      , tuTranslation = "ambiguous because of the time shift"
      , tuNoteForSender =
        Just [int||_There is a timeshift in #{shownTZ True} around the specified \
             time and this particular timestamp can be possible with \
             different offsets, please define the offset explicitly._|]
      , tuNoteForOthers =
        Just [int||_There is a time zone shift in #{shownTZ False} around that \
             time, and this particular timestamp can be possible with \
             different offsets._|]
      }
  TRTUInvalid implicitSenderTimezone tzLabel -> do
    let shownTZ = shownTimezone implicitSenderTimezone tzLabel
    Left $ TranslationPair
      { tuTimeRef = trText timeRef
      , tuTranslation = "invalid because of the time shift"
      , tuNoteForSender =
        Just [int||_There is a timeshift in #{shownTZ True} around the specified \
             time and this particular timestamp does not exist, \
             please define the offset explicitly._|]
      , tuNoteForOthers =
        Just [int||_There is a time zone shift in #{shownTZ False} around that \
             time, and this particular timestamp does not exist._|]
      }

  TRTUInvalidTimeZoneAbbrev abbrev ->
    Left $ TranslationPair
      (trText timeRef)
      [int||contains invalid timezone abbreviation: #{abbrev}|]
      Nothing -- TODO: We can find some abbreviations that are similar to
              -- what the sender defined and that we know about
      Nothing
  where
    shownTimezone :: Bool -> TZLabel -> Bool -> Builder
    shownTimezone implicitSenderTimezone tzLabel forSender
      | implicitSenderTimezone =
          if forSender
          then [int||your timezone (#{tzLabel})|]
          else [int||the sender's timezone (#{tzLabel})|]
      | otherwise = [int||#{tzLabel}|]

renderUserTime :: TZLabel -> UTCTime -> UTCTime -> String
renderUserTime tzLabel now refTime = do
  let tzInfo = TZI.fromLabel tzLabel
      currentUserLocalTime = TZT.tzTimeLocalTime $ TZT.fromUTC tzInfo now
      refUserLocalTime = TZT.tzTimeLocalTime $ TZT.fromUTC tzInfo refTime
      sameYear = theYearIsTheSame currentUserLocalTime refUserLocalTime
      yearFormat = if sameYear then "" else ", %Y"
      -- example:
      -- 18:23, Monday, December 26, 2016
      format = "%H:%M, %A, %B %d" <> yearFormat
  formatTime defaultTimeLocale format refUserLocalTime

theYearIsTheSame :: LocalTime -> LocalTime -> Bool
theYearIsTheSame t1 t2 = do
  let YearMonthDay y1 _ _ = localDay t1
  let YearMonthDay y2 _ _ = localDay t2
  y1 == y2
