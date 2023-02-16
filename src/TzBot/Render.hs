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
import Data.Time.Compat
  (Day, LocalTime, UTCTime, ZonedTime(zonedTimeToLocalTime), defaultTimeLocale, formatTime,
  minutesToTimeZone, utcToZonedTime)
import Data.Time.TZInfo (TZInfo(tziIdentifier))
import Data.Time.TZInfo qualified as TZI
import Data.Time.TZTime qualified as TZT
import Data.Time.Zones.All (TZLabel)
import Text.Interpolation.Nyan (int, rmode')

import TzBot.Instances ()
import TzBot.Slack.API (Mrkdwn(Mrkdwn), User(..))
import TzBot.Slack.API.Block
import TzBot.TZ (TimeShift(..), checkForTimeshifts, findLastTimeshift)
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
chooseNote (SenderFlag sender) =
  if sender then tuNoteForSender else tuNoteForOthers

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

-- | Render a template that can be later specialized to different users.
renderTemplate :: ModalFlag -> UTCTime -> User -> NE.NonEmpty TimeReference -> Template
renderTemplate modalFlag now sender timeRefs =
  Template $ NE.map (renderEphemeralMessageTranslationPair modalFlag sender)
    $ attach (timeReferenceToUTC (uTz sender) now) timeRefs

-- | This flag defines whether time references are rendered to be shown
-- in a chat or in a modal view.
newtype ModalFlag = ModalFlag Bool

asForModalM, asForMessageM :: ModalFlag
asForModalM = ModalFlag True
asForMessageM = ModalFlag False

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
        Just $ T.strip $ renderTimeshiftWarning
          (trText timeRef)
          trsUtcResult
          trsTzInfo
          userTzLabel
          totalTimeshifts

  mbRenderedUserTime <&> \rt -> TranslationPair
    (renderOriginalTimeRef sender timeRef trsOriginalDate)
    rt
    mbTimeshiftNote
    mbTimeshiftNote

-- | Render warning when conversion was successful but not sure in inferred date.
renderTimeshiftWarning
  :: Text
  -> UTCTime
  -> TZI.TZInfo
  -> TZLabel
  -> [TimeShift]
  -> Text
renderTimeshiftWarning refText inferredTime mentionedTzInfo receiversTzLabel ts = do
  let renderedInferredTime = renderTimeGeneral "%d %B %Y" mentionedTzInfo inferredTime
      newLine = "\n"
  [int|n|
    _Warning: We inferred that "#{refText}" refers to #{renderedInferredTime}
    in #{TZI.tziIdentifier mentionedTzInfo} and
    converted it to #{receiversTzLabel}, but there is a time change near this date_:

    #{T.intercalate newLine $ map renderTimeshiftWarningUnit ts}

    _Beware that if this inference is not correct and the sender meant a different date,
    the conversion may not be accurate._
    |]
  where

  renderTimeshiftWarningUnit :: TimeShift -> Text
  renderTimeshiftWarningUnit ts@TimeShift {..} = do
    let prevOffsetTZInfo = tzInfoFromOffset tsBefore
    [int|Dn|
  • _At #{renderTimeGeneral "%H:%M, %d %B %Y" prevOffsetTZInfo tsShiftUtc} \
in #{tziIdentifier tsTzInfo}, the clocks are turned #{shiftInfo ts}_.
    |]
    where

    shiftInfo :: TimeShift -> Builder
    shiftInfo TimeShift {..} = do
      let offsetDiff = tsBefore `diffOffsetMinutes` tsAfter
      let (hoursDiff, mbMinutesDiff) = absDivMinutesByHours offsetDiff
          direction = if offsetDiff > 0 then "backward" else "forward" :: Text
          shownMinutesDiff = mbMinutesDiff <&> \d ->
            [int|| (and #{d} minutes)|] :: Builder
      [int||#{direction} #{hoursDiff} hour(s)#{shownMinutesDiff}|]

-- | Render representation of initially mentioned time, possibly
-- with some appended inferred context like date or timezone/offset.
renderOriginalTimeRef :: User -> TimeReference -> Day -> Text
renderOriginalTimeRef sender timeRef originalDay = do
  let mbSenderTimeZone :: Maybe Builder
      mbSenderTimeZone = case trLocationRef timeRef of
        Just (TimeZoneAbbreviationRef TimeZoneAbbreviationInfo {..}) -> do
          guard $ notElem tzaiAbbreviation ["UTC", "GMT"]
          Just [int||, #{tzaiFullName} (#{tzaiOffsetMinutes}) |]
        Just _ -> Nothing
        Nothing -> Just [int|| in #{uTz sender}|]
      mbShownOriginalDate = case trDateRef timeRef of
        Just (DayOfMonthRef _ (Just _)) -> Nothing
        _ -> do
          let format = ", %d %B %Y"
          Just $ formatTime defaultTimeLocale format originalDay
  [int||"#{trText timeRef}"#{mbShownOriginalDate}#{mbSenderTimeZone}|]

-- | Given a timeshift in a timezone, define the last local time in
-- the previous offset (first in the tuple) and
-- the first local time in the next offset (second in the tuple).
getTimeshiftBoundaries :: TimeShift -> (LocalTime, LocalTime)
getTimeshiftBoundaries TimeShift {..} = do
  let getBoundaryLocalTime :: Offset -> LocalTime
      getBoundaryLocalTime o = zonedTimeToLocalTime $
        utcToZonedTime (minutesToTimeZone $ unOffset o) tsShiftUtc

      beforeTimeshift = getBoundaryLocalTime tsBefore
      afterTimeshift = getBoundaryLocalTime tsAfter
  (beforeTimeshift, afterTimeshift)

renderOnOverlap :: User -> TimeReference -> OverlapInfo -> TranslationPair
renderOnOverlap sender timeRef OverlapInfo {..} = do
  let TimeShiftErrorInfo {..} = oiErrorInfo
      firstOffset = tztimeOffset oiFirstOccurrence
      secondOffset = tztimeOffset oiSecondOccurrence
      (hoursDiff, mbMinutesDiff) = absDivMinutesByHours $
        firstOffset `diffOffsetMinutes` secondOffset
      shownMinutesDiff = mbMinutesDiff <&> \d ->
        [int|| (and #{d} minutes)|] :: Builder
      isImplicitSenderTimezone = isNothing $ trLocationRef timeRef

      timeshift = findLastTimeshift oiSecondOccurrence
      (beforeTimeshift, afterTimeshift) = getTimeshiftBoundaries timeshift
      tzIdentifier = tziIdentifier $ TZT.tzTimeTZInfo oiFirstOccurrence

  let shownTZ = shownTimezoneOnErrors isImplicitSenderTimezone tzIdentifier

  let commonPart forSender = [int|n|
        At #{renderTimeOfDay beforeTimeshift}, the clocks are turned backward #{hoursDiff}
        hour(s)#{shownMinutesDiff} to #{renderTimeOfDay afterTimeshift} and this
        particular time occurs twice in #{shownTZ forSender}, first with the
        offset #{firstOffset} and then with #{secondOffset}.
        |] :: Builder
      additionForSender = [int|n|
        Please edit your message or write a new one and specify an offset explicitly.
        |] :: Builder
  TranslationPair
    { tuTimeRef = renderOriginalTimeRef sender timeRef tseiOriginalDate
    , tuTranslation = "Ambiguous time"
    , tuNoteForSender = Just [int||_#{commonPart True} #{additionForSender}_|]
    , tuNoteForOthers = Just [int||_#{commonPart False}_|]
    }

renderOnGap :: User -> TimeReference -> GapInfo -> TranslationPair
renderOnGap sender timeRef GapInfo {..} = do
  let TimeShiftErrorInfo {..} = giErrorInfo
      prevOffset = tztimeOffset giPreviousTime
      nextOffset = tztimeOffset giNextTime
      (hoursDiff, mbMinutesDiff) = absDivMinutesByHours $
        prevOffset `diffOffsetMinutes` nextOffset

      shownMinutesDiff = mbMinutesDiff <&> \d ->
        [int|| (and #{d} minutes)|] :: Builder

      isImplicitSenderTimezone = isNothing $ trLocationRef timeRef
      timeshift = findLastTimeshift giNextTime

      (beforeGap, afterGap) = getTimeshiftBoundaries timeshift

      tzIdentifier = tziIdentifier $ TZT.tzTimeTZInfo giPreviousTime

  let shownTZ = shownTimezoneOnErrors isImplicitSenderTimezone tzIdentifier
  let commonPart forSender = [int|n|
        At #{renderTimeOfDay beforeGap}, the clocks are turned forward #{hoursDiff} hour(s)#{shownMinutesDiff}
        to #{renderTimeOfDay afterGap} and this
        particular time does not occur in #{shownTZ forSender}.
        |] :: Builder
      additionForSender = [int|n|
        Please edit your message or write a new one and amend the time. Did you mean
        #{renderTimeOfDay $ TZT.tzTimeLocalTime giPreviousTime} or
        #{renderTimeOfDay $ TZT.tzTimeLocalTime giNextTime} instead?
        |] :: Builder

  TranslationPair
    { tuTimeRef = renderOriginalTimeRef sender timeRef tseiOriginalDate
    , tuTranslation = "Invalid time"
    , tuNoteForSender = Just [int||_#{commonPart True} #{additionForSender}_|]
    , tuNoteForOthers = Just [int||_#{commonPart False}_|]
    }

-- | Divide minutes by hours, producing integer amount of hours and possibly
-- minutes in case of non-zero remainder.
absDivMinutesByHours :: Int -> (Int, Maybe Int)
absDivMinutesByHours (abs -> offsetDiff) = do
  let (hoursDiff, minutesDiff) = offsetDiff `divMod` (let minPerHr = 60 in minPerHr)
      mbMinutesDiff = guard (minutesDiff /= 0) >> Just minutesDiff
  (hoursDiff, mbMinutesDiff)

-- | Specify timezone belonging if present.
shownTimezoneOnErrors :: Bool -> TZI.TZIdentifier -> Bool -> Builder
shownTimezoneOnErrors implicitSenderTimezone tzLabel forSender
  | implicitSenderTimezone =
      if forSender
      then [int||your timezone (#{tzLabel})|]
      else [int||the sender's timezone (#{tzLabel})|]
  | otherwise = [int||#{tzLabel}|]

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
  TRTUAmbiguous overlapInfo ->
    Left $ renderOnOverlap sender timeRef overlapInfo
  TRTUInvalid gapInfo ->
    Left $ renderOnGap sender timeRef gapInfo
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

renderTimeOfDay :: LocalTime -> String
renderTimeOfDay = formatTime defaultTimeLocale "%H:%M"

renderTimeGeneral :: String -> TZI.TZInfo -> UTCTime -> String
renderTimeGeneral format tzInfo refTime = do
  let refUserLocalTime = TZT.tzTimeLocalTime $ TZT.fromUTC tzInfo refTime
  formatTime defaultTimeLocale format refUserLocalTime

-- example:
-- 18:23, Monday, 26 December 2016
renderUserTime :: TZLabel -> UTCTime -> String
renderUserTime tzLabel = renderTimeGeneral "%H:%M, %A, %d %B %Y" (TZI.fromLabel tzLabel)
