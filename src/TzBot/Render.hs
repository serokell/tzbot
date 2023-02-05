-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Render
  ( -- * Types
    TranslationPair (..)
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
  , SenderFlag
  , asForSenderS
  , asForOthersS

  , ModalFlag
  , asForModalM
  , asForMessageM
  ) where

import Universum

import Data.Aeson (ToJSON)
import Data.List.NonEmpty qualified as NE
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Builder (Builder, fromText, singleton, toLazyText)
import Data.Time.Compat (Day, UTCTime, defaultTimeLocale, formatTime)
import Data.Time.TZInfo qualified as TZI
import Data.Time.TZTime qualified as TZT
import Data.Time.Zones.All (TZLabel)
import Text.Interpolation.Nyan (int, rmode')

import TzBot.Instances ()
import TzBot.Slack.API (Mrkdwn(Mrkdwn), User(..))
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

--------
-- Notes
-- | Used for choosing between two notes
newtype SenderFlag = SenderFlag Bool

asForSenderS, asForOthersS :: SenderFlag
asForSenderS = SenderFlag True
asForOthersS = SenderFlag False

chooseNote :: SenderFlag -> TranslationPair -> Maybe Text
chooseNote (SenderFlag sender) = if sender then tuNoteForSender else tuNoteForOthers

--------
concatTranslationPair :: SenderFlag -> TranslationPair -> Builder
concatTranslationPair sender t@TranslationPair {..} = do
  let rightNote = chooseNote sender t
  let note = maybe "" (("\n" <>) . fromText) rightNote
  [int||#{tuTimeRef}:  #{tuTranslation}#{note}|]

-- Render generic
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
  T.toStrict . toLazyText . fold . NE.toList
    . NE.map ((<> singleton '\n') . concatTranslationPair sender)

-- Render Slack block
renderSlackBlocks :: SenderFlag -> Maybe TranslationPairs -> [Block]
renderSlackBlocks forSender =
  maybe [noRefsFoundSection]
    (intercalate [BDivider divider] . NE.toList . NE.map mkTranslationBlocks)
  where
    noRefsFoundSection = BSection $ markdownSection $ Mrkdwn noRefsFoundMsg
    mkTranslationBlocks :: TranslationPair -> [Block]
    mkTranslationBlocks timeRef = do
      let t = (Mrkdwn $ tuTimeRef timeRef, Mrkdwn $ tuTranslation timeRef)
          mbNote = chooseNote forSender timeRef
          translationBlock = BSection $ fieldsSection Nothing $ NE.singleton t
          mkNoteBlock note = BSection $ markdownSection $ Mrkdwn note
      withMaybe mbNote [translationBlock] $ \note -> [translationBlock, mkNoteBlock note]

renderTemplate :: ModalFlag -> UTCTime -> User -> NE.NonEmpty TimeReference -> Template
renderTemplate modalFlag now sender timeRefs =
  Template $ NE.map (renderEphemeralMessageTranslationPair modalFlag sender)
    $ attach (timeReferenceToUTC (uTz sender) now) timeRefs

--
newtype ModalFlag = ModalFlag Bool

asForModalM, asForMessageM :: ModalFlag
asForModalM = ModalFlag True
asForMessageM = ModalFlag False

--
renderOnSuccess
  :: ModalFlag
  -> User
  -> TimeReference
  -> TimeRefSuccess
  -> User
  -> Maybe TranslationPair
renderOnSuccess (ModalFlag forModal) sender timeRef TimeRefSuccess {..} user = do
  let userTzLabel = uTz user
      renderedUserTime = do
        let q = renderUserTime userTzLabel trsUtcResult
        [int||#{q} in #{userTzLabel}|] :: Text
      mbRenderedUserTime = case trsEithTzOffset of
        Right _ -> Just renderedUserTime
        Left refTzLabel ->
          if refTzLabel /= userTzLabel
          then Just renderedUserTime
          else do
            let isNotSender = ((/=) `on` uId) sender user
                shouldShowThisTranslation = isNotSender || forModal
            guard shouldShowThisTranslation
            Just "You are in this timezone"
  mbRenderedUserTime <&> \rt -> TranslationPair
    (getOriginalTimeRef sender timeRef trsOriginalDate)
    rt
    Nothing
    Nothing

getOriginalTimeRef :: User -> TimeReference -> Day -> Text
getOriginalTimeRef sender timeRef originalDay = do
  let mbSenderTimeZone :: Maybe Builder
      mbSenderTimeZone = case trLocationRef timeRef of
        Just _ -> Nothing
        Nothing -> Just [int|| in #{uTz sender}|]
      mbShownOriginalDate = case trDateRef timeRef of
        Just (DayOfMonthRef _ (Just _)) -> Nothing
        _ -> do
          let format = ", %d %B %Y"
          Just $ formatTime defaultTimeLocale format originalDay
  [int||"#{trText timeRef}"#{mbShownOriginalDate}#{mbSenderTimeZone}|]

-- Under `Left` we collect errors, that should be shown to all users (including sender),
-- and under `Right` we collect valid time references that should be rendered differently
-- for each user.
renderEphemeralMessageTranslationPair
  :: ModalFlag
  -> User
  -> (TimeReference, TimeReferenceToUTCResult)
  -> EitherTemplateUnit
renderEphemeralMessageTranslationPair modalFlag sender (timeRef, result) = case result of
  TRTUSuccess timeRefSuc ->
    Right $ renderOnSuccess modalFlag sender timeRef timeRefSuc
  TRTUAmbiguous TimeShiftErrorInfo {..} -> do
    let shownTZ = shownTimezone tseiIsImplicitSenderTimezone tseiRefTimeZone
    Left $ TranslationPair
      { tuTimeRef = getOriginalTimeRef sender timeRef tseiOriginalDate
      , tuTranslation = "Ambiguous because of the time shift"
      , tuNoteForSender =
        Just [int||_There is a timeshift in #{shownTZ True} around the specified \
             time and this particular timestamp can be possible with \
             different offsets, please define the offset explicitly._|]
      , tuNoteForOthers =
        Just [int||_There is a time zone shift in #{shownTZ False} around that \
             time, and this particular timestamp can be possible with \
             different offsets._|]
      }
  TRTUInvalid TimeShiftErrorInfo {..} -> do
    let shownTZ = shownTimezone tseiIsImplicitSenderTimezone tseiRefTimeZone
    Left $ TranslationPair
      { tuTimeRef = getOriginalTimeRef sender timeRef tseiOriginalDate
      , tuTranslation = "Invalid because of the time shift"
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

renderUserTime :: TZLabel -> UTCTime -> String
renderUserTime tzLabel refTime = do
  let tzInfo = TZI.fromLabel tzLabel
      refUserLocalTime = TZT.tzTimeLocalTime $ TZT.fromUTC tzInfo refTime
      -- example:
      -- 18:23, Monday, 26 December 2016
      format = "%H:%M, %A, %d %B %Y"
  formatTime defaultTimeLocale format refUserLocalTime
