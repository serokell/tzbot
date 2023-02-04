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
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder, fromText, singleton, toLazyText)
import Data.Time.Compat (Day, UTCTime, defaultTimeLocale, formatTime)
import Data.Time.TZInfo qualified as TZI
import Data.Time.TZTime qualified as TZT
import Data.Time.Zones.All (TZLabel)
import Text.Interpolation.Nyan (int, rmode')

import TzBot.Instances ()
import TzBot.Slack.API (Mrkdwn(Mrkdwn), User(..))
import TzBot.Slack.API.Block
import TzBot.TZ (TimeShift(..), checkForTimeshifts)
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
  TL.toStrict . toLazyText . fold . NE.toList
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
renderOnSuccess (ModalFlag forModal) sender timeRef timeRefSucess@TimeRefSuccess {..} user = do
  let userTzLabel = uTz user
      renderedUserTime = do
        let q = renderUserTime userTzLabel trsUtcResult
        [int||#{q} in #{userTzLabel}|] :: Text
      mbRefTzLabel = getTzLabelMaybe (uTz sender) timeRef
      mbRenderedUserTime = case mbRefTzLabel of
        Nothing -> Just renderedUserTime
        Just refTzLabel ->
          if refTzLabel /= userTzLabel
          then Just renderedUserTime
          else do
            let isNotSender = ((/=) `on` uId) sender user
                shouldShowThisTranslation = isNotSender || forModal
            guard shouldShowThisTranslation
            Just "You are in this timezone"
      totalTimeshifts = checkForTimeshifts timeRef timeRefSucess userTzLabel
      mbTimeshiftNote = do
        guard $ not $ null totalTimeshifts
        Just $ T.strip $ getTimeshiftWarning
          (trText timeRef)
          trsUtcResult
          trsTzInfo
          userTzLabel
          totalTimeshifts

  mbRenderedUserTime <&> \rt -> TranslationPair
    (getOriginalTimeRef sender timeRef trsOriginalDate)
    rt
    mbTimeshiftNote
    mbTimeshiftNote

getTimeshiftWarningUnit :: TimeShift -> Text
getTimeshiftWarningUnit ts@TimeShift {..} = do
  let prevOffsetTZInfo = tzInfoFromOffset tsBefore
  [int|Dn|
  • _At #{renderTimeGeneral "%H:%M, %d %B %Y" prevOffsetTZInfo tsShiftUtc} in #{TZI.tziIdentifier tsTzInfo},
the clocks are turned #{shiftInfo ts}_.
    |]

shiftInfo :: TimeShift -> Builder
shiftInfo TimeShift {..} = do
  let (positive, hoursDiff, mbMinutesDiff) = getOffsetDiff tsBefore tsAfter
      direction = if positive then "backward" else "forward" :: Text
      shownMinutesDiff = mbMinutesDiff <&> \d ->
        [int|| (and #{d} minutes)|] :: Builder
  [int||#{direction} #{hoursDiff} hour(s)#{shownMinutesDiff}|]

getOffsetDiff :: Offset -> Offset -> (Bool, Int, Maybe Int)
getOffsetDiff o1 o2 = do
  let diff = o1 `diffOffsetMinutes` o2
      oDiff = abs diff
      (hoursDiff, minutesDiff) = oDiff `divMod` (let minPerHr = 60 in minPerHr)
      mbMinutesDiff = guard (minutesDiff /= 0) >> Just minutesDiff
  (diff >= 0, hoursDiff, mbMinutesDiff)

getTimeshiftWarning :: Text -> UTCTime -> TZI.TZInfo -> TZLabel -> [TimeShift] -> Text
getTimeshiftWarning refText inferredTime mentionedTzInfo receiversTzLabel ts = do
  let renderedInferredTime = renderTimeGeneral "%d %B %Y" mentionedTzInfo inferredTime
      newLine = "\n"
  [int|n|
    _Warning: We inferred that "#{refText}" refers to #{renderedInferredTime} in #{TZI.tziIdentifier mentionedTzInfo} and
    converted it to #{receiversTzLabel}, but there is a time change near this date_:

    #{T.intercalate newLine $ map getTimeshiftWarningUnit ts}

    _Beware that if this inference is not correct and the sender meant a different date,
    the conversion may not be accurate._
    |]

getOriginalTimeRef :: User -> TimeReference -> Day -> Text
getOriginalTimeRef sender timeRef originalDay = do
  let mbSenderTimeZone :: Maybe Builder
      mbSenderTimeZone = case trLocationRef timeRef of
        Just (TimeZoneAbbreviationRef TimeZoneAbbreviationInfo {..}) -> do
          guard $ not $ tzaiAbbreviation `elem` ["UTC", "GMT"]
          Just [int||, #{tzaiFullName} (#{tzaiOffsetMinutes}) |]
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
    let shownTZ = shownTimezone tseiRefTimeZone
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
    let shownTZ = shownTimezone tseiRefTimeZone
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

  TRTUInvalidTimeZoneAbbrev UnknownTimeZoneAbbrev {..} -> do
    let mbNeCandidates = nonEmpty utzaCandidates
        mbCandidatesNoteForSender = flip fmap mbNeCandidates \neCandidates -> do
          let renderedCandidates =
                T.intercalate ", " $
                  map unTimeZoneAbbreviation $ NE.toList neCandidates
          [int||_Maybe you meant: #{renderedCandidates}_|]
    Left $ TranslationPair
      [int||"#{trText timeRef}"|]
      [int||Contains unrecognized timezone abbreviation: #{utzaAbbrev}|]
      mbCandidatesNoteForSender
      Nothing
  where
    shownTimezone :: TZLabel -> Bool -> Builder
    shownTimezone tzLabel forSender
      | implicitSenderTimezone =
          if forSender
          then [int||your timezone (#{tzLabel})|]
          else [int||the sender's timezone (#{tzLabel})|]
      | otherwise = [int||#{tzLabel}|]
      where
        implicitSenderTimezone = isNothing $ trLocationRef timeRef

renderTimeGeneral :: String -> TZI.TZInfo -> UTCTime -> String
renderTimeGeneral format tzInfo refTime = do
  let refUserLocalTime = TZT.tzTimeLocalTime $ TZT.fromUTC tzInfo refTime
  formatTime defaultTimeLocale format refUserLocalTime

-- example:
-- 18:23, Monday, 26 December 2016
renderUserTime :: TZLabel -> UTCTime -> String
renderUserTime tzLabel = renderTimeGeneral "%H:%M, %A, %d %B %Y" (TZI.fromLabel tzLabel)
