-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Render
  ( -- * Types
    ConversionPair (..)
  , ConversionPairs
  , Template

    -- * Render generic
  , renderAllConversionPairs

    -- * Render text
  , renderAll
  , joinConversionPairs

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

import TzPrelude

import Data.Aeson (ToJSON)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder, fromText, singleton, toLazyText)
import Data.Time
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
import TzBot.TimeReference
import TzBot.TZ (ClockChange(..), checkForClockChanges, findLastClockChange)
import TzBot.Util

-- Types

-- We use `Left` to keep conversion errors (invalid/ambiguous time,
-- invalid offset abbreviation) and they are common for all users,
-- and valid conversions (`Right`) depend on the receiver timezone.
type EitherTemplateUnit = Either ConversionPair (User -> Maybe ConversionPair)

data ConversionPair = ConversionPair
  { cpTimeRef :: Text
    -- ^ The piece of the original message containing a time reference
  , cpConversion :: Text
    -- ^ Valid or invalid conversion of the time reference
  , cpNoteForSender :: Maybe Text
    -- ^ Additional optional information for the sender
  , cpNoteForOthers :: Maybe Text
    -- ^ Additional optional information for others (not the sender)
  } deriving stock (Eq, Show, Generic)
    deriving ToJSON via RecordWrapper ConversionPair

type ConversionPairs = NE.NonEmpty ConversionPair

newtype Template = Template { unTemplate :: NE.NonEmpty EitherTemplateUnit }

--------
-- Notes
-- | Used for choosing between two notes
newtype SenderFlag = SenderFlag Bool

asForSenderS, asForOthersS :: SenderFlag
asForSenderS = SenderFlag True
asForOthersS = SenderFlag False

chooseNote :: SenderFlag -> ConversionPair -> Maybe Text
chooseNote (SenderFlag sender) =
  if sender then cpNoteForSender else cpNoteForOthers

--------
concatConversionPair :: SenderFlag -> ConversionPair -> Builder
concatConversionPair sender cp = do
  let rightNote = chooseNote sender cp
  let note = maybe "" (("\n" <>) . fromText) rightNote
  [int||#{cpTimeRef cp}:  #{cpConversion cp}#{note}|]

-- Render generic
renderAllConversionPairs :: User -> Template -> Maybe ConversionPairs
renderAllConversionPairs user =
  nonEmpty . mapMaybe (either Just ($ user)) . NE.toList . unTemplate

-- | We show this message because we may have not found
-- any time references while actually there were.
noRefsFoundMsg :: Text
noRefsFoundMsg = "No time references found."

-- Render text
renderAll :: User -> Template -> Maybe Text
renderAll user =
  fmap (joinConversionPairs asForSenderS) . renderAllConversionPairs user

joinConversionPairs :: SenderFlag -> ConversionPairs -> Text
joinConversionPairs sender =
  TL.toStrict . toLazyText . fold . NE.toList
    . NE.map ((<> singleton '\n') . concatConversionPair sender)

-- Render Slack block
renderSlackBlocks :: SenderFlag -> Maybe ConversionPairs -> [Block]
renderSlackBlocks forSender =
  maybe [noRefsFoundSection]
    (intercalate [BDivider divider] . NE.toList . NE.map mkConversionBlocks)
  where
    noRefsFoundSection = BSection $ markdownSection $ Mrkdwn noRefsFoundMsg
    mkConversionBlocks :: ConversionPair -> [Block]
    mkConversionBlocks timeRef = do
      let t = (Mrkdwn $ cpTimeRef timeRef, Mrkdwn $ cpConversion timeRef)
          mbNote = chooseNote forSender timeRef
          conversionBlock = BSection $ fieldsSection Nothing $ NE.singleton t
          mkNoteBlock note = BSection $ markdownSection $ Mrkdwn note
      withMaybe mbNote [conversionBlock] $ \note -> [conversionBlock, mkNoteBlock note]

-- | Render a template that can be later specialized to different users.
renderTemplate :: ModalFlag -> UTCTime -> User -> NE.NonEmpty TimeReference -> Template
renderTemplate modalFlag now sender timeRefs =
  Template $ NE.map (renderEphemeralMessageConversionPair modalFlag sender)
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
  -> Maybe ConversionPair
renderOnSuccess (ModalFlag forModal) sender timeRef timeRefSucess user = do
  let userTzLabel = uTz user
      renderedUserTime = do
        let q = renderUserTime userTzLabel timeRefSucess.trsUtcResult
        [int||#{q} in #{userTzLabel}|] :: Text
      mbRefTzLabel = getTzLabelMaybe (uTz sender) timeRef
      mbRenderedUserTime = case mbRefTzLabel of
        Nothing -> Just renderedUserTime
        Just refTzLabel ->
          if refTzLabel /= userTzLabel
          then Just renderedUserTime
          else do
            let isNotSender = ((/=) `on` uId) sender user
                shouldShowThisConversion = isNotSender || forModal
            guard shouldShowThisConversion
            Just "You are in this timezone"
      totalClockChanges = checkForClockChanges timeRef timeRefSucess userTzLabel
      mbClockChangeWarning = do
        guard $ not $ null totalClockChanges
        Just $ T.strip $ renderClockChangeWarning
          (trText timeRef)
          timeRefSucess.trsUtcResult
          timeRefSucess.trsTzInfo
          userTzLabel
          totalClockChanges

  mbRenderedUserTime <&> \rt -> ConversionPair
    (renderOriginalTimeRef sender timeRef timeRefSucess.trsOriginalDate)
    rt
    mbClockChangeWarning
    mbClockChangeWarning

-- | Render warning when conversion was successful but not sure in inferred date.
renderClockChangeWarning
  :: Text
  -> UTCTime
  -> TZI.TZInfo
  -> TZLabel
  -> [ClockChange]
  -> Text
renderClockChangeWarning refText inferredTime mentionedTzInfo receiversTzLabel ts = do
  let renderedInferredTime = renderTimeGeneral "%d %B %Y" mentionedTzInfo inferredTime
      newLine = "\n"
  [int|n|
    _Warning: We inferred that "#{refText}" refers to #{renderedInferredTime}
    in #{TZI.tziIdentifier mentionedTzInfo} and
    converted it to #{receiversTzLabel}, but there is a time change near this date_:

    #{T.intercalate newLine $ map renderClockChangeWarningUnit ts}

    _Beware that if this inference is not correct and the sender meant a different date,
    the conversion may not be accurate._
    |]
  where

  renderClockChangeWarningUnit :: ClockChange -> Text
  renderClockChangeWarningUnit cc = do
    let prevOffsetTZInfo = tzInfoFromOffset cc.ccBefore
    [int|Dn|
  • _At #{renderTimeGeneral "%H:%M, %d %B %Y" prevOffsetTZInfo (ccUTCTime cc)} \
in #{ccTzIdentifier cc}, the clocks are turned #{clockChangeInfo}_.
    |]
    where

    clockChangeInfo :: Builder
    clockChangeInfo = do
      let offsetDiff = cc.ccBefore `diffOffsetMinutes` cc.ccAfter
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
        Just (TimeZoneAbbreviationRef abbrevationInfo) -> do
          guard $ notElem abbrevationInfo.tzaiAbbreviation ["UTC", "GMT"]
          Just [int||, #{tzaiFullName abbrevationInfo} (#{tzaiOffsetMinutes abbrevationInfo}) |]
        Just _ -> Nothing
        Nothing -> Just [int|| in #{uTz sender}|]
      mbShownOriginalDate = case trDateRef timeRef of
        Just (DayOfMonthRef _ (Just _)) -> Nothing
        _ -> do
          let format = ", %d %B %Y"
          Just $ formatTime defaultTimeLocale format originalDay
  [int||"#{trText timeRef}"#{mbShownOriginalDate}#{mbSenderTimeZone}|]

-- | Given a clock change in a timezone, define the last local time in
-- the previous offset (first in the tuple) and
-- the first local time in the next offset (second in the tuple).
getClockChangeBoundaries :: ClockChange -> (LocalTime, LocalTime)
getClockChangeBoundaries cc = do
  let getBoundaryLocalTime :: Offset -> LocalTime
      getBoundaryLocalTime o =
        cc.ccUTCTime
          & utcToZonedTime (minutesToTimeZone o.unOffset)
          & zonedTimeToLocalTime
  (getBoundaryLocalTime cc.ccBefore, getBoundaryLocalTime cc.ccAfter)

renderOnOverlap :: User -> TimeReference -> OverlapInfo -> ConversionPair
renderOnOverlap sender timeRef overlapInfo = do
  let firstOffset = tztimeOffset overlapInfo.oiFirstOccurrence
      secondOffset = tztimeOffset overlapInfo.oiSecondOccurrence
      (hoursDiff, mbMinutesDiff) = absDivMinutesByHours $
        firstOffset `diffOffsetMinutes` secondOffset
      shownMinutesDiff = mbMinutesDiff <&> \d ->
        [int|| (and #{d} minutes)|] :: Builder
      isImplicitSenderTimezone = isNothing $ trLocationRef timeRef

      clockChange = findLastClockChange overlapInfo.oiSecondOccurrence
      (beforeClockChange, afterClockChange) = getClockChangeBoundaries clockChange
      tzIdentifier = tziIdentifier $ TZT.tzTimeTZInfo overlapInfo.oiFirstOccurrence

  let shownTZ = shownTimezoneOnErrors isImplicitSenderTimezone tzIdentifier

  let commonPart forSender = [int|n|
        At #{renderTimeOfDay beforeClockChange}, the clocks are turned backward #{hoursDiff}
        hour(s)#{shownMinutesDiff} to #{renderTimeOfDay afterClockChange} and this
        particular time occurs twice in #{shownTZ forSender}, first with the
        offset #{firstOffset} and then with #{secondOffset}.
        |] :: Builder
      additionForSender = [int|n|
        Please edit your message or write a new one and specify an offset explicitly.
        |] :: Builder
  ConversionPair
    { cpTimeRef = renderOriginalTimeRef sender timeRef overlapInfo.oiErrorInfo.cceiOriginalDate
    , cpConversion = "Ambiguous time"
    , cpNoteForSender = Just [int||_#{commonPart True} #{additionForSender}_|]
    , cpNoteForOthers = Just [int||_#{commonPart False}_|]
    }

renderOnGap :: User -> TimeReference -> GapInfo -> ConversionPair
renderOnGap sender timeRef gapInfo = do
  let prevOffset = tztimeOffset gapInfo.giPreviousTime
      nextOffset = tztimeOffset gapInfo.giNextTime
      (hoursDiff, mbMinutesDiff) = absDivMinutesByHours $
        prevOffset `diffOffsetMinutes` nextOffset

      shownMinutesDiff = mbMinutesDiff <&> \d ->
        [int|| (and #{d} minutes)|] :: Builder

      isImplicitSenderTimezone = isNothing $ trLocationRef timeRef
      clockChange = findLastClockChange gapInfo.giNextTime

      (beforeGap, afterGap) = getClockChangeBoundaries clockChange

      tzIdentifier = tziIdentifier $ TZT.tzTimeTZInfo gapInfo.giPreviousTime

  let shownTZ = shownTimezoneOnErrors isImplicitSenderTimezone tzIdentifier
  let commonPart forSender = [int|n|
        At #{renderTimeOfDay beforeGap}, the clocks are turned forward #{hoursDiff} hour(s)#{shownMinutesDiff}
        to #{renderTimeOfDay afterGap} and this
        particular time does not occur in #{shownTZ forSender}.
        |] :: Builder
      additionForSender = [int|n|
        Please edit your message or write a new one and amend the time. Did you mean
        #{renderTimeOfDay $ TZT.tzTimeLocalTime $ giPreviousTime gapInfo} or
        #{renderTimeOfDay $ TZT.tzTimeLocalTime $ giNextTime gapInfo} instead?
        |] :: Builder

  ConversionPair
    { cpTimeRef = renderOriginalTimeRef sender timeRef gapInfo.giErrorInfo.cceiOriginalDate
    , cpConversion = "Invalid time"
    , cpNoteForSender = Just [int||_#{commonPart True} #{additionForSender}_|]
    , cpNoteForOthers = Just [int||_#{commonPart False}_|]
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
renderEphemeralMessageConversionPair
  :: ModalFlag
  -> User
  -> (TimeReference, TimeReferenceToUTCResult)
  -> EitherTemplateUnit
renderEphemeralMessageConversionPair modalFlag sender (timeRef, result) = case result of
  TRTUSuccess timeRefSuc ->
    Right $ renderOnSuccess modalFlag sender timeRef timeRefSuc
  TRTUAmbiguous overlapInfo ->
    Left $ renderOnOverlap sender timeRef overlapInfo
  TRTUInvalid gapInfo ->
    Left $ renderOnGap sender timeRef gapInfo
  TRTUInvalidTimeZoneAbbrev UnknownTimeZoneAbbrev {utzaCandidates, utzaAbbrev} -> do
    let mbNeCandidates = nonEmpty utzaCandidates
        mbCandidatesNoteForSender = flip fmap mbNeCandidates \neCandidates -> do
          let renderedCandidates =
                T.intercalate ", " $
                  map unTimeZoneAbbreviation $ NE.toList neCandidates
          [int||_Maybe you meant: #{renderedCandidates}_|]
    Left $ ConversionPair
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
