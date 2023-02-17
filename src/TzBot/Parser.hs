-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Parser
  ( parseTimeRefs
  ) where

import Universum hiding (many, toList, try)

import Data.Char (isUpper)
import Data.List qualified as L
import Data.Map qualified as M
import Data.String.Conversions (cs)
import Data.Text qualified as T
import Data.Text.Metrics (damerauLevenshteinNorm)
import Data.Time (DayOfWeek(..))
import Data.Time.Calendar.Compat (DayOfMonth, MonthOfYear, Year)
import Data.Time.LocalTime (TimeOfDay(..))
import Data.Time.Zones.All (TZLabel, tzNameLabelMap)
import Glider.NLP.Tokenizer (Token(..), tokenize)
import Text.Interpolation.Nyan (int, rmode')
import Text.Megaparsec hiding (Token)

import TzBot.Instances ()
import TzBot.TimeReference
import TzBot.Util
import TzBot.Util qualified as CI

type TzParser = Parsec Void [Token]

{- | Parses time references from an input string.

>>> parseTimeRefs "let's meet tuesday at 10am"
[TimeReference {trText = "tuesday at 10am", trTimeOfDay = 10:00:00, trDateRef = Just (DayOfWeekRef Tuesday), trLocationRef = Nothing}]

>>> parseTimeRefs "i can do it at 3pm MDT"
[TimeReference {trText = "at 3pm MDT", trTimeOfDay = 15:00:00, trDateRef = Nothing, trLocationRef = Just (TimeZoneAbbreviationRef (TimeZoneAbbreviationInfo {tzaiAbbreviation = "MDT", tzaiOffsetMinutes = -360, tzaiFullName = "Mountain Daylight Time (North America)"}))}]

>>> parseTimeRefs "how about between 2pm and 3pm?"
[TimeReference {trText = "2pm", trTimeOfDay = 14:00:00, trDateRef = Nothing, trLocationRef = Nothing},TimeReference {trText = "3pm", trTimeOfDay = 15:00:00, trDateRef = Nothing, trLocationRef = Nothing}]

>>> parseTimeRefs "Does 10am work for you?"
[TimeReference {trText = "10am", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Nothing}]

>>> parseTimeRefs "That doesn't work for me, what about 10:30 AM?"
[TimeReference {trText = "10:30 AM", trTimeOfDay = 10:30:00, trDateRef = Nothing, trLocationRef = Nothing}]

>>> parseTimeRefs "I can only be there at 16:00"
[TimeReference {trText = "at 16:00", trTimeOfDay = 16:00:00, trDateRef = Nothing, trLocationRef = Nothing}]

>>> parseTimeRefs "10am tomorrow"
[TimeReference {trText = "10am tomorrow", trTimeOfDay = 10:00:00, trDateRef = Just (DaysFromToday 1), trLocationRef = Nothing}]

>>> parseTimeRefs "today at 3pm"
[TimeReference {trText = "today at 3pm", trTimeOfDay = 15:00:00, trDateRef = Just (DaysFromToday 0), trLocationRef = Nothing}]

>>> parseTimeRefs "10am in 2 days"
[TimeReference {trText = "10am in 2 days", trTimeOfDay = 10:00:00, trDateRef = Just (DaysFromToday 2), trLocationRef = Nothing}]

>>> parseTimeRefs "tuesday at 3pm"
[TimeReference {trText = "tuesday at 3pm", trTimeOfDay = 15:00:00, trDateRef = Just (DayOfWeekRef Tuesday), trLocationRef = Nothing}]

>>> parseTimeRefs "at 3pm on tuesday"
[TimeReference {trText = "at 3pm on tuesday", trTimeOfDay = 15:00:00, trDateRef = Just (DayOfWeekRef Tuesday), trLocationRef = Nothing}]

>>> parseTimeRefs "at 11am on the 4th "
[TimeReference {trText = "at 11am on the 4th", trTimeOfDay = 11:00:00, trDateRef = Just (DayOfMonthRef 4 Nothing), trLocationRef = Nothing}]

>>> parseTimeRefs "at 11am on the 4th of April"
[TimeReference {trText = "at 11am on the 4th of April", trTimeOfDay = 11:00:00, trDateRef = Just (DayOfMonthRef 4 (Just (4,Nothing))), trLocationRef = Nothing}]

>>> parseTimeRefs "at 11am on April 4"
[TimeReference {trText = "at 11am on April 4", trTimeOfDay = 11:00:00, trDateRef = Just (DayOfMonthRef 4 (Just (4,Nothing))), trLocationRef = Nothing}]

>>> parseTimeRefs "at 11am on 4 April"
[TimeReference {trText = "at 11am on 4 April", trTimeOfDay = 11:00:00, trDateRef = Just (DayOfMonthRef 4 (Just (4,Nothing))), trLocationRef = Nothing}]

>>> parseTimeRefs "9am in europe/london"
[TimeReference {trText = "9am in europe/london", trTimeOfDay = 09:00:00, trDateRef = Nothing, trLocationRef = Just (TimeZoneRef Europe__London)}]

>>> parseTimeRefs "2pm CST"
[TimeReference {trText = "2pm CST", trTimeOfDay = 14:00:00, trDateRef = Nothing, trLocationRef = Just (TimeZoneAbbreviationRef (TimeZoneAbbreviationInfo {tzaiAbbreviation = "CST", tzaiOffsetMinutes = -360, tzaiFullName = "Central Standard Time (North America)"}))}]

>>> parseTimeRefs "10am UTC+03:00"
[TimeReference {trText = "10am UTC+03:00", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Just (OffsetRef 180)}]

>>> parseTimeRefs "10am UTC+3"
[TimeReference {trText = "10am UTC+3", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Just (OffsetRef 180)}]

>>> parseTimeRefs "10am UTC +3"
[TimeReference {trText = "10am UTC +3", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Just (OffsetRef 180)}]

>>> parseTimeRefs "10am UTC-3"
[TimeReference {trText = "10am UTC-3", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Just (OffsetRef (-180))}]

>>> parseTimeRefs "10am UTC -3"
[TimeReference {trText = "10am UTC -3", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Just (OffsetRef (-180))}]

>>> parseTimeRefs "10am UTC-blabla"
[TimeReference {trText = "10am", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Nothing}]

>>> parseTimeRefs "Let's meet between 10am and 11:30am"
[TimeReference {trText = "10am", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Nothing},TimeReference {trText = "11:30am", trTimeOfDay = 11:30:00, trDateRef = Nothing, trLocationRef = Nothing}]

>>> parseTimeRefs "35pm"
[]

>>> parseTimeRefs "35pmkek"
[]

>>> parseTimeRefs "15:00pm"
[TimeReference {trText = "15:00pm", trTimeOfDay = 15:00:00, trDateRef = Nothing, trLocationRef = Nothing}]

>>> parseTimeRefs "13:00 Nov 06"
[TimeReference {trText = "13:00 Nov 06", trTimeOfDay = 13:00:00, trDateRef = Just (DayOfMonthRef 6 (Just (11,Nothing))), trLocationRef = Nothing}]

>>> parseTimeRefs "11:12:13 nOv 1"
[TimeReference {trText = "11:12:13 nOv 1", trTimeOfDay = 11:12:00, trDateRef = Just (DayOfMonthRef 1 (Just (11,Nothing))), trLocationRef = Nothing}]

>>> parseTimeRefs "13:00 06 Nov"
[TimeReference {trText = "13:00 06 Nov", trTimeOfDay = 13:00:00, trDateRef = Just (DayOfMonthRef 6 (Just (11,Nothing))), trLocationRef = Nothing}]

>>> parseTimeRefs "10am 11am"
[TimeReference {trText = "10am", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Nothing},TimeReference {trText = "11am", trTimeOfDay = 11:00:00, trDateRef = Nothing, trLocationRef = Nothing}]

>>> parseTimeRefs "1:12:23pm"
[TimeReference {trText = "1:12:23pm", trTimeOfDay = 13:12:00, trDateRef = Nothing, trLocationRef = Nothing}]

>>> parseTimeRefs "12am the day after tomorrow"
[TimeReference {trText = "12am the day after tomorrow", trTimeOfDay = 12:00:00, trDateRef = Just (DaysFromToday 2), trLocationRef = Nothing}]

>>> parseTimeRefs "12am day after tomorrow"
[TimeReference {trText = "12am day after tomorrow", trTimeOfDay = 12:00:00, trDateRef = Just (DaysFromToday 2), trLocationRef = Nothing}]

>>> parseTimeRefs "12am 3 days ahead"
[TimeReference {trText = "12am 3 days ahead", trTimeOfDay = 12:00:00, trDateRef = Just (DaysFromToday 3), trLocationRef = Nothing}]

>>> parseTimeRefs "9:3am"
[]

>>> parseTimeRefs "13:00 06 nov"
[TimeReference {trText = "13:00 06 nov", trTimeOfDay = 13:00:00, trDateRef = Just (DayOfMonthRef 6 (Just (11,Nothing))), trLocationRef = Nothing}]

>>> parseTimeRefs "13:00 nov 06"
[TimeReference {trText = "13:00 nov 06", trTimeOfDay = 13:00:00, trDateRef = Just (DayOfMonthRef 6 (Just (11,Nothing))), trLocationRef = Nothing}]

>>> parseTimeRefs "today,10am"
[TimeReference {trText = "today,10am", trTimeOfDay = 10:00:00, trDateRef = Just (DaysFromToday 0), trLocationRef = Nothing}]

>>> parseTimeRefs "today ,   10am"
[TimeReference {trText = "today , 10am", trTimeOfDay = 10:00:00, trDateRef = Just (DaysFromToday 0), trLocationRef = Nothing}]

>>> parseTimeRefs "today,   10am"
[TimeReference {trText = "today, 10am", trTimeOfDay = 10:00:00, trDateRef = Just (DaysFromToday 0), trLocationRef = Nothing}]

>>> parseTimeRefs "today ,10am"
[TimeReference {trText = "today ,10am", trTimeOfDay = 10:00:00, trDateRef = Just (DaysFromToday 0), trLocationRef = Nothing}]

>>> parseTimeRefs "10am aMeRiCa/Argentina/Buenos_Aires"
[TimeReference {trText = "10am aMeRiCa/Argentina/Buenos_Aires", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Just (TimeZoneRef America__Argentina__Buenos_Aires)}]

>>> parseTimeRefs "10am America/North_Dakota/New_Salem"
[TimeReference {trText = "10am America/North_Dakota/New_Salem", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Just (TimeZoneRef America__North_Dakota__New_Salem)}]

>>> parseTimeRefs "10am America/port-au-Prince"
[TimeReference {trText = "10am America/port-au-Prince", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Just (TimeZoneRef America__Port_au_Prince)}]

>>> parseTimeRefs "10am MSKC"
[TimeReference {trText = "10am MSKC", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Just (UnknownTimeZoneAbbreviationRef (UnknownTimeZoneAbbrev {utzaAbbrev = "MSKC", utzaCandidates = ["MSK"]}))}]

>>> parseTimeRefs "10am KSMC"
[TimeReference {trText = "10am KSMC", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Just (UnknownTimeZoneAbbreviationRef (UnknownTimeZoneAbbrev {utzaAbbrev = "KSMC", utzaCandidates = []}))}]

>>> parseTimeRefs "10am KSMc"
[TimeReference {trText = "10am", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Nothing}]

>>> parseTimeRefs "10am K"
[TimeReference {trText = "10am", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Nothing}]

>>> parseTimeRefs "10am KAMAZN"
[TimeReference {trText = "10am", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Nothing}]

>>> parseTimeRefs "01:03 6 November America/Winnipeg"
[TimeReference {trText = "01:03 6 November America/Winnipeg", trTimeOfDay = 01:03:00, trDateRef = Just (DayOfMonthRef 6 (Just (11,Nothing))), trLocationRef = Just (TimeZoneRef America__Winnipeg)}]

>>> parseTimeRefs "01:03 6 November 2022 America/Winnipeg"
[TimeReference {trText = "01:03 6 November 2022 America/Winnipeg", trTimeOfDay = 01:03:00, trDateRef = Just (DayOfMonthRef 6 (Just (11,Just 2022))), trLocationRef = Just (TimeZoneRef America__Winnipeg)}]

>>> parseTimeRefs "01:03 2022 6 November America/Winnipeg"
[TimeReference {trText = "01:03 2022 6 November America/Winnipeg", trTimeOfDay = 01:03:00, trDateRef = Just (DayOfMonthRef 6 (Just (11,Just 2022))), trLocationRef = Just (TimeZoneRef America__Winnipeg)}]

>>> parseTimeRefs "7.30 pm "
[TimeReference {trText = "7.30 pm", trTimeOfDay = 19:30:00, trDateRef = Nothing, trLocationRef = Nothing}]

>>> parseTimeRefs "7.30"
[]

>>> parseTimeRefs "19h "
[TimeReference {trText = "19h", trTimeOfDay = 19:00:00, trDateRef = Nothing, trLocationRef = Nothing}]

>>> parseTimeRefs "19h01 "
[TimeReference {trText = "19h01", trTimeOfDay = 19:01:00, trDateRef = Nothing, trLocationRef = Nothing}]

>>> parseTimeRefs "7:30pm 03/08/2022"
[TimeReference {trText = "7:30pm 03/08/2022", trTimeOfDay = 19:30:00, trDateRef = Just (DayOfMonthRef 3 (Just (8,Just 2022))), trLocationRef = Nothing}]

>>> parseTimeRefs "7:30pm 3-08-2022"
[TimeReference {trText = "7:30pm 3-08-2022", trTimeOfDay = 19:30:00, trDateRef = Just (DayOfMonthRef 3 (Just (8,Just 2022))), trLocationRef = Nothing}]

>>> parseTimeRefs "7:30pm 3.08.2022"
[TimeReference {trText = "7:30pm 3.08.2022", trTimeOfDay = 19:30:00, trDateRef = Just (DayOfMonthRef 3 (Just (8,Just 2022))), trLocationRef = Nothing}]

>>> parseTimeRefs "7:30pm 2022/08/3"
[TimeReference {trText = "7:30pm 2022/08/3", trTimeOfDay = 19:30:00, trDateRef = Just (DayOfMonthRef 3 (Just (8,Just 2022))), trLocationRef = Nothing}]

>>> parseTimeRefs "2022.8.03 7:30 pm  "
[TimeReference {trText = "2022.8.03 7:30 pm", trTimeOfDay = 19:30:00, trDateRef = Just (DayOfMonthRef 3 (Just (8,Just 2022))), trLocationRef = Nothing}]

>>> parseTimeRefs "7:30pm 2022.8.03 America/Havana"
[TimeReference {trText = "7:30pm 2022.8.03 America/Havana", trTimeOfDay = 19:30:00, trDateRef = Just (DayOfMonthRef 3 (Just (8,Just 2022))), trLocationRef = Just (TimeZoneRef America__Havana)}]

>>> parseTimeRefs "tomorrow 10am -11 am"
[TimeReference {trText = "tomorrow 10am", trTimeOfDay = 10:00:00, trDateRef = Just (DaysFromToday 1), trLocationRef = Nothing},TimeReference {trText = "tomorrow 11 am", trTimeOfDay = 11:00:00, trDateRef = Just (DaysFromToday 1), trLocationRef = Nothing}]

>>> parseTimeRefs "tomorrow 10am /  11 am"
[TimeReference {trText = "tomorrow 10am", trTimeOfDay = 10:00:00, trDateRef = Just (DaysFromToday 1), trLocationRef = Nothing},TimeReference {trText = "tomorrow 11 am", trTimeOfDay = 11:00:00, trDateRef = Just (DaysFromToday 1), trLocationRef = Nothing}]

>>> parseTimeRefs "between 10am and 11:30am UTC"
[TimeReference {trText = "10am UTC", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Just (TimeZoneAbbreviationRef (TimeZoneAbbreviationInfo {tzaiAbbreviation = "UTC", tzaiOffsetMinutes = 0, tzaiFullName = "UTC"}))},TimeReference {trText = "11:30am UTC", trTimeOfDay = 11:30:00, trDateRef = Nothing, trLocationRef = Just (TimeZoneAbbreviationRef (TimeZoneAbbreviationInfo {tzaiAbbreviation = "UTC", tzaiOffsetMinutes = 0, tzaiFullName = "UTC"}))}]

>>> parseTimeRefs "Let's go on Wednesday at 10:00 or 11:00."
[TimeReference {trText = "on Wednesday at 10:00", trTimeOfDay = 10:00:00, trDateRef = Just (DayOfWeekRef Wednesday), trLocationRef = Nothing},TimeReference {trText = "on Wednesday 11:00", trTimeOfDay = 11:00:00, trDateRef = Just (DayOfWeekRef Wednesday), trLocationRef = Nothing}]

>>> parseTimeRefs "10-11pm tomorrow works for me"
[TimeReference {trText = "10pm tomorrow", trTimeOfDay = 22:00:00, trDateRef = Just (DaysFromToday 1), trLocationRef = Nothing},TimeReference {trText = "11pm tomorrow", trTimeOfDay = 23:00:00, trDateRef = Just (DaysFromToday 1), trLocationRef = Nothing}]

>>> parseTimeRefs "How about 10:00 or 11:00 pm tomorrow?"
[TimeReference {trText = "10:00 pm tomorrow", trTimeOfDay = 22:00:00, trDateRef = Just (DaysFromToday 1), trLocationRef = Nothing},TimeReference {trText = "11:00 pm tomorrow", trTimeOfDay = 23:00:00, trDateRef = Just (DaysFromToday 1), trLocationRef = Nothing}]

>>> parseTimeRefs "7.30-8.30pm"
[TimeReference {trText = "7.30pm", trTimeOfDay = 19:30:00, trDateRef = Nothing, trLocationRef = Nothing},TimeReference {trText = "8.30pm", trTimeOfDay = 20:30:00, trDateRef = Nothing, trLocationRef = Nothing}]

>>> parseTimeRefs "7.30am-8.30pm tomorrow"
[TimeReference {trText = "7.30am tomorrow", trTimeOfDay = 07:30:00, trDateRef = Just (DaysFromToday 1), trLocationRef = Nothing},TimeReference {trText = "8.30pm tomorrow", trTimeOfDay = 20:30:00, trDateRef = Just (DaysFromToday 1), trLocationRef = Nothing}]

>>> parseTimeRefs "7.30-8.30"
[]

-}
parseTimeRefs :: Text -> [TimeReference]
parseTimeRefs =
  -- TODO use better error handling
  map matchedToPlain
    . fromMaybe []
    . parseMaybe timeRefsParser
    -- time reference can be either at the beginning or after a space
    . (Whitespace :)
    . tokenize

-- | Parser for multiple 'TimeReference' s.
--
-- This looks for all of them in the input and ignores everything surrounding.
timeRefsParser :: TzParser [TimeReferenceMatched]
timeRefsParser = choice'
  [ do
      tr <- try timeRefConjugParser
      trs <- timeRefsParser
      return $ tr <> trs
  , anySingle >> timeRefsParser
  , takeRest >> pure []
  ]

-- | Parses entries like @between 10am and 11am@ or
-- @10am-11am on thursday or 1pm-2pm on wednesday@
timeRefConjugParser :: TzParser [TimeReferenceMatched]
timeRefConjugParser = do
  firstConjugComponent <- timeRefParser
  let conjugParser conjWord = do
        optional' space
        _ <- word' conjWord
        -- no space here before `timeRefParser` requires a space before the contents
        secondConjugComponent <- timeRefParser
        pure $ unifyConjugComponents $ firstConjugComponent <> secondConjugComponent

      unifyConjugComponents :: [TimeReferenceMatched] -> [TimeReferenceMatched]
      unifyConjugComponents lst = do
        let getUnique :: Eq a => (TimeReferenceMatched -> Maybe a) -> Maybe a
            getUnique getter = do
              let many = L.nub $ mapMaybe getter lst
              case many of
                [item] -> Just item
                _ -> Nothing
        let locRef = getUnique trLocationRef
            dateRef = getUnique trDateRef
        -- TODO: use lenses
        flip map lst $
          (whenJustFunc locRef \l tr -> addLocationIfMissing l tr)
            . whenJustFunc dateRef \d tr -> addDateIfMissing d tr

  choice'
    -- note that and/or can be parsed either as conjugations or as "hyphens",
    -- in the second case am/pm context is also shared
    [ conjugParser "and"
    , conjugParser "or"
    , pure firstConjugComponent
    ]

addLocationIfMissing
  :: Matched LocationReference
  -> TimeReferenceMatched
  -> TimeReferenceMatched
addLocationIfMissing l tr =
  if isNothing (trLocationRef tr)
  then tr { trLocationRef = Just l, trText = [int||#{trText tr} (#{mtText l})|] }
  else tr

addDateIfMissing
  :: Matched DateReference
  -> TimeReferenceMatched
  -> TimeReferenceMatched
addDateIfMissing d tr =
  if isNothing (trDateRef tr)
  then tr { trDateRef = Just d, trText = [int||#{trText tr} (#{mtText d})|] }
  else tr

-- | Parses coupled 'TimeReference's, collecting the source text.
timeRefParser :: TzParser [TimeReferenceMatched]
timeRefParser = do
  _ <- space
  (precText, precBuilder) <- match $ fromMaybe builderInit <$> do
    res <- optional' (builderParser False builderInit)
    optional' spacedComma
    pure res
  timeEntry <- timeEntryParser
  (afterText, builder) <- match $ fromMaybe builderInit <$> optional' (builderParser True precBuilder)
  let trLocationRef = trbLocRef builder
      trDateRef = trbDateRef builder
  let mkTrText refText =
        T.concat [concatTokens precText, refText, concatTokens afterText]
  let mkTimeReference todWithText = TimeReference
        { trText = mkTrText $ mtText todWithText
        , trTimeOfDay = mtValue todWithText
        , trDateRef
        , trLocationRef
        }
  pure $ map mkTimeReference case timeEntry of
    TESingle todwt -> [todwt]
    TEPair todwt todwt' -> [todwt, todwt']

----------------------------------------------------------------------------
---- Collecting of optional time contexts
----------------------------------------------------------------------------
data ContextBuilder = ContextBuilder
  { trbDateRef :: Maybe (Matched DateReference)
  , trbLocRef  :: Maybe (Matched LocationReference)
  } deriving stock (Show, Eq)

builderInit :: ContextBuilder
builderInit = ContextBuilder Nothing Nothing

data SumContextBuilder
  = SCBDate (Matched DateReference)
  | SCBLocRef (Matched LocationReference)

sumBuilderParser :: TzParser SumContextBuilder
sumBuilderParser =
  choice'
    [ SCBDate . matched <$> match dateRefParser
    , SCBLocRef . matched <$> match locRefParser
    ]

builderParser :: Bool -> ContextBuilder -> TzParser ContextBuilder
builderParser allowSpace b = do
  sumB <- optional'
    (when allowSpace (void $ optional' spacedComma) >> matched <$> match sumBuilderParser)
  case fmap mtValue sumB of
    Just (SCBDate dr) -> do
      when (isJust $ trbDateRef b) empty
      builderParser True b { trbDateRef = Just dr }
    Just (SCBLocRef lr) -> do
      when (isJust $ trbLocRef b) empty
      builderParser True b { trbLocRef = Just lr }
    Nothing -> pure b

----------------------------------------------------------------------------
matched :: ([Token], a) -> Matched a
matched (ts, val) = Matched (concatTokens ts) val

data TimeEntry
  = TESingle (Matched TimeOfDay)
  -- ^ E.g. @10am@
  | TEPair (Matched TimeOfDay) (Matched TimeOfDay)
  -- ^ E.g. @10am-11am@

-- | Parses either a single time of day or a pair with shared
-- date, location and am/pm contexts.
timeEntryParser :: TzParser TimeEntry
timeEntryParser = do
  let todWithTextParser = matched <$> match timeOfDayParser
  firstRef <- todWithTextParser
  let delimitedPair :: TzParser a -> TzParser TimeEntry
      delimitedPair delim = do
        optional' space
        delim
        optional' space
        secondRef <- todWithTextParser
        let getIsAm
              :: Matched (Maybe (TimeOfDay, Maybe $ Matched Bool), Bool -> TimeOfDay)
              -> Maybe (Matched Bool)
            getIsAm ref = fst (mtValue ref) >>= snd
        let isAmOptions = mapMaybe getIsAm [firstRef, secondRef]
        let applyIsAm
              :: Matched Bool
              -> Matched (Maybe (TimeOfDay, Maybe $ Matched Bool), Bool -> TimeOfDay)
              -> Matched TimeOfDay
            applyIsAm isAm ref = do
              let shouldAppend = isNothing $ getIsAm ref
              whenFunc shouldAppend (modifyText (<> mtText isAm)) $ fmap (($ mtValue isAm) . snd) ref
            extractDefaultResult :: Matched (Maybe (TimeOfDay, a), b) -> Maybe (Matched TimeOfDay)
            extractDefaultResult ref = traverse (fmap fst . fst) ref
        case isAmOptions of
          [isAm] -> pure $ (TEPair `on` applyIsAm isAm) firstRef secondRef
          _ -> maybe empty pure $ TEPair <$> extractDefaultResult firstRef <*> extractDefaultResult secondRef

  choice'
    [ delimitedPair (punct '-')
    , delimitedPair (punct '/')
    , delimitedPair (word' "or")
    , delimitedPair (word' "and")
    , TESingle <$> traverse (\(mbRes, _) -> maybe empty (pure . fst) mbRes) firstRef
    ]

-- | Parses a 'TimeOfDay', returning a template for `timeEntryParser`, which can later
-- provide another am/pm context that we can infer by default.
timeOfDayParser
  :: TzParser
      (Maybe
        (TimeOfDay -- if parsed correctly, this is a value with the default am/pm context applied
        , Maybe (Matched Bool) -- if am/pm context parsed, return it here
        )
      , Bool -> TimeOfDay -- if it turns out that another am/pm context should be applied,
                          -- provide construction function for that case
      )
timeOfDayParser = do
  _ <- optional' (relationPreposition >> space)
  hour <- hourParser

  (maybeMin, isAmRequired) <- choice'
    [ do  tryHour <- anyWord
          case T.uncons tryHour of
            Nothing -> empty
            Just (h, after)
              | h `elem` ['h', 'H'] -> case after of
                "" -> pure (Nothing, False)
                nstr -> pure (readMinutes nstr, False)
              | otherwise -> empty

    , do  isPoint <- token' $ \case
            Punctuation '.' -> Just True
            Punctuation ':' -> Just False
            _ -> Nothing
          mins <- minuteParser
          if isPoint
          then pure (Just mins, True)
          else do
            let secondParser = minuteParser
            _ <- optional' $ punct ':' >> secondParser
            pure (Just mins, False)
    , pure (Nothing, True)
    ]

  let mkTime isAm = do
        let todSec = 0
            todHour
              | isAm = hour
              -- pm here
              | hour < 12 = hour + 12
              -- ignore pm if hour > 12
              | otherwise = hour
            todMin = fromMaybe 0 maybeMin
        TimeOfDay {..}

  mbIsAm <- optional' $ matched <$> match isAmParser
  pure . (,mkTime) $ case mbIsAm of
    Just isAm -> Just (mkTime $ mtValue isAm, Just isAm)
    Nothing ->
      if isAmRequired
      then Nothing
      else Just (mkTime True, Nothing)

isAmParser :: TzParser Bool
isAmParser = optional' space >>
  (word' "AM" >> pure True) <|> (word' "PM" >> pure False)

--------------------------------------------------------------------------------
-- DateReference
--------------------------------------------------------------------------------

dateRefParser :: TzParser DateReference
dateRefParser = choice'
  [ daysFromTodayParser
  , dayOfWeekRefParser
  , dateParser
  ]

daysFromTodayParser :: TzParser DateReference
daysFromTodayParser = fmap DaysFromToday . choice' $
  [ word' "today"     >> pure 0
  , word' "tomorrow"  >> pure 1
  , word' "yesterday" >> pure -1
  , do
      _ <- optional' (word' "the" >> space)
      matchSomeWords $ "day" :| ["after", "tomorrow"]
      pure 2
  , do
      _ <- optional' (word' "in" >> space)
      n <- number
      space
      word' "days"
      _ <- optional' (space >> word' "ahead")
      pure n
  ]

matchSomeWords :: NonEmpty Text -> TzParser ()
matchSomeWords (t :| ts) = word' t >> mapM_ (\s -> space >> word' s) ts

dayOfWeekRefParser :: TzParser DateReference
dayOfWeekRefParser = do
  _ <- optional' (relationPreposition >> space)
  dayName <- anyWord
  DayOfWeekRef <$> withStorage daysOfWeekStorage dayName

dateParser :: TzParser DateReference
dateParser = choice' [dateParserVerbose, dateParserFormat]

-- TODO: maybe this should be configurable by user
dateParserFormat :: TzParser DateReference
dateParserFormat = do
  let format1 :: TzParser a -> TzParser DateReference
      format1 delim = do
        y <- yearParser
        m <- delim >> monthOfYearNumberParser
        (d, _) <- delim >> dayOfMonthParser
        pure $ DayOfMonthRef d $ Just (m, (Just y))

      format2 :: TzParser a -> TzParser DateReference
      format2 delim = do
        (d, _) <- dayOfMonthParser
        m <- delim >> monthOfYearNumberParser
        y <- delim >> yearParser
        pure $ DayOfMonthRef d $ Just (m, Just y)

  choice' [f (punct delim) | f <- [format1, format2], delim <- ['/', '-', '.']]

dateParserVerbose :: TzParser DateReference
dateParserVerbose = do
  precYear <- optional' ( do y <- yearParser; spacedComma; pure y )
  _ <- optional' (relationPreposition >> space)
  (dayOfMonth, monthOfYear) <- choice' $
    [ do
        _ <- optional' (word' "the" >> space)
        (dayOfMonth, hasSuffix) <- dayOfMonthParser
        _ <- optional' (space >> word' "of")
        monthOfYear <- optional' (space >> monthOfYearParser)
        -- otherwise we can consider an unrelated number
        -- as a valid day of month
        guard $ hasSuffix || isJust monthOfYear
        pure (dayOfMonth, monthOfYear)
    , do
        monthOfYear <- Just <$> monthOfYearParser
        spacedComma
        optional' (word' "the" >> space)
        dayOfMonth <- fst <$> dayOfMonthParser
        pure (dayOfMonth, monthOfYear)
    ]
  year <- withMaybe precYear
    (optional' (optional' spacedComma >> yearParser))
    (pure . Just)
  pure $ DayOfMonthRef dayOfMonth $ fmap (, year) monthOfYear

dayOfMonthParser :: TzParser (DayOfMonth, Bool)
dayOfMonthParser = do
  n <- number
  guard (n < 32)
  hasSuffix <- isJust <$>
    optional' (word' "st" <|> word' "nd" <|> word' "rd" <|> word' "th")
  pure (n, hasSuffix)

monthOfYearNumberParser :: TzParser MonthOfYear
monthOfYearNumberParser = do
  n <- number
  guard (n <= 12)
  pure n

monthOfYearParser :: TzParser MonthOfYear
monthOfYearParser = do
  monName <- anyWord
  withStorage monthStorage monName

yearParser :: TzParser Year
yearParser = do
  year <- number
  guard $ year < 2100 && year > 1900
  pure $ fromIntegral @Int @Integer year

--------------------------------------------------------------------------------
-- LocationReference
--------------------------------------------------------------------------------

locRefParser :: TzParser LocationReference
locRefParser = do
  _ <- optional' (word' "in" >> space)
  choice' [offsetRefParser, tzAbbrRefParser, tzRefParser]

tzRefParser :: TzParser LocationReference
tzRefParser = do
  res <- sepBy1' word'Hyphen'Underscore (punct '/')
  let total = concatTokens res
  TimeZoneRef <$> withStorage tzNameLabelStorage total

offsetRefParser :: TzParser LocationReference
offsetRefParser = do
  oneOf $ map Word ["UTC", "GMT"]
  optional' space
  (sign :: Int) <- choice
    [ punct '+' >> pure 1
    , punct '-' >> pure (-1)
    ]
  hours <- hourParser
  maybeMins <- optional' minuteColonParser
  let minutesTotal = 60 * hours + fromMaybe 0 maybeMins
  pure $ OffsetRef (Offset $ sign * minutesTotal)

tzAbbrRefParser :: TzParser LocationReference
tzAbbrRefParser = do
  abbr <- anyWord
  let similarityThreshold = 0.65 :: Ratio Int
  -- Check if this word is a recognized timezone abbreviation.
  --
  -- If it isn't, check whether the user likely meant to refer to some
  -- timezone abbreviation, but possibly made a typo or used an abbreviation we don't support.
  -- If so, and assuming it was a typo, suggest abbreviations similar enough to what the user entered.
  -- E.g., if they entered "WETS", they likely meant either "WET" or "WEST".
  res <- choice'
    [ do info <- withStorage knownTimeZoneAbbreviations abbr
         pure $ TimeZoneAbbreviationRef info
    , UnknownTimeZoneAbbreviationRef <$> do
        guard $ isPossibleTimezoneAbbrev abbr
        let predicate cand = do
              let norm = damerauLevenshteinNorm abbr $
                    unTimeZoneAbbreviation cand
              norm > similarityThreshold
        let candidates =
              filter predicate . map tzaiAbbreviation $ knownTimeZoneAbbreviations'
        pure $ UnknownTimeZoneAbbrev (TimeZoneAbbreviation abbr) candidates
    ]
  -- That's added to avoid interference with UTC+... and GMT+... syntax.
  notFollowedBy $ choice' $ map punct ['+', '-']
  pure res

isPossibleTimezoneAbbrev :: Text -> Bool
isPossibleTimezoneAbbrev w =
  T.all isUpper w
    && T.length w >= 2
    && T.length w <= 5
    && not (w `elem` ["AM", "PM", "OR", "AND"])

--------------------------------------------------------------------------------
-- Storages
--------------------------------------------------------------------------------

addCut3Option :: (Text, b) -> [(Text, b)]
addCut3Option t = [t, first (T.take 3) t]

daysOfWeekStorage :: CI.CIStorage DayOfWeek
daysOfWeekStorage = CI.fromList $ concatMap addCut3Option
  [ ("Monday", Monday)
  , ("Tuesday", Tuesday)
  , ("Wednesday", Wednesday)
  , ("Thursday", Thursday)
  , ("Friday", Friday)
  , ("Saturday", Saturday)
  , ("Sunday", Sunday)
  ]

monthStorage :: CI.CIStorage MonthOfYear
monthStorage = CI.fromList $ concatMap addCut3Option
  [ ("January", 1)
  , ("February", 2)
  , ("March", 3)
  , ("April", 4)
  , ("May", 5)
  , ("June", 6)
  , ("July", 7)
  , ("August", 8)
  , ("September", 9)
  , ("October", 10)
  , ("November", 11)
  , ("December", 12)
  ]

tzNameLabelStorage :: CI.CIStorage TZLabel
tzNameLabelStorage = CI.fromList . map (first cs) . M.toList $ tzNameLabelMap

knownTimeZoneAbbreviations :: CI.CIStorage TimeZoneAbbreviationInfo
knownTimeZoneAbbreviations =
  CI.fromList . map (\tzAbbr ->
    (unTimeZoneAbbreviation $ tzaiAbbreviation tzAbbr, tzAbbr)) $
      knownTimeZoneAbbreviations'

-- | A subset of https://en.wikipedia.org/wiki/List_of_time_zone_abbreviations
knownTimeZoneAbbreviations' :: [TimeZoneAbbreviationInfo]
knownTimeZoneAbbreviations' =
  -- TODO: add more tz abbreviations.
  --
  -- NOTE: Remember to update `docs/timezone_abbreviations.md` when making changes to this list.
  [ TimeZoneAbbreviationInfo "UTC" 0 "UTC"
  , TimeZoneAbbreviationInfo "GMT" 0 "GMT"
  , TimeZoneAbbreviationInfo "HST"   (hours -10)       "Hawaii-Aleutian Standard Time"
  , TimeZoneAbbreviationInfo "HDT"   (hours -09)       "Hawaii-Aleutian Daylight Time"
  , TimeZoneAbbreviationInfo "PST"   (hours -08)       "Pacific Standard Time (North America)"
  , TimeZoneAbbreviationInfo "PDT"   (hours -07)       "Pacific Daylight Time (North America)"
  , TimeZoneAbbreviationInfo "MST"   (hours -07)       "Mountain Standard Time (North America)"
  , TimeZoneAbbreviationInfo "MDT"   (hours -06)       "Mountain Daylight Time (North America)"
  , TimeZoneAbbreviationInfo "CST"   (hours -06)       "Central Standard Time (North America)"
  , TimeZoneAbbreviationInfo "CDT"   (hours -05)       "Central Daylight Time (North America)"
  , TimeZoneAbbreviationInfo "EST"   (hours -05)       "Eastern Standard Time (North America)"
  , TimeZoneAbbreviationInfo "EDT"   (hours -04)       "Eastern Daylight Time (North America)"
  , TimeZoneAbbreviationInfo "AST"   (hours -04)       "Atlantic Standard Time"
  , TimeZoneAbbreviationInfo "ADT"   (hours -03)       "Atlantic Daylight Time"
  , TimeZoneAbbreviationInfo "AMT"   (hours -04)       "Amazon Time"
  , TimeZoneAbbreviationInfo "AMST"  (hours -03)       "Amazon Summer Time"
  , TimeZoneAbbreviationInfo "CLT"   (hours -04)       "Chile Standard Time"
  , TimeZoneAbbreviationInfo "CLST"  (hours -03)       "Chile Summer Time"
  , TimeZoneAbbreviationInfo "BRT"   (hours -03)       "Brasilia Time"
  , TimeZoneAbbreviationInfo "BRST"  (hours -02)       "Brasilia Summer Time"
  , TimeZoneAbbreviationInfo "WET"   (hours  00)       "Western European Time"
  , TimeZoneAbbreviationInfo "WEST"  (hours  01)       "Western European Summer Time"
  , TimeZoneAbbreviationInfo "BST"   (hours  01)       "British Summer Time"
  , TimeZoneAbbreviationInfo "CET"   (hours  01)       "Central European Time"
  , TimeZoneAbbreviationInfo "CEST"  (hours  02)       "Central European Summer Time"
  , TimeZoneAbbreviationInfo "WAT"   (hours  01)       "West Africa Time"
  , TimeZoneAbbreviationInfo "WAST"  (hours  02)       "West Africa Summer Time"
  , TimeZoneAbbreviationInfo "CAT"   (hours  02)       "Central Africa Time"
  , TimeZoneAbbreviationInfo "SAST"  (hours  02)       "South African Standard Time"
  , TimeZoneAbbreviationInfo "EET"   (hours  02)       "Eastern European Time"
  , TimeZoneAbbreviationInfo "EEST"  (hours  03)       "Eastern European Summer Time"
  , TimeZoneAbbreviationInfo "MSK"   (hours  03)       "Moscow Time"
  , TimeZoneAbbreviationInfo "TRT"   (hours  03)       "Turkey Time"
  , TimeZoneAbbreviationInfo "GET"   (hours  04)       "Georgia Standard Time"
  , TimeZoneAbbreviationInfo "IST"   (hours  05 + 30)  "India Standard Time"
  , TimeZoneAbbreviationInfo "AWST"  (hours  08)       "Australian Western Standard Time"
  , TimeZoneAbbreviationInfo "AWDT"  (hours  09)       "Australian Western Daylight Time"
  , TimeZoneAbbreviationInfo "ACWST" (hours  08 + 45)  "Australian Central Western Standard Time"
  , TimeZoneAbbreviationInfo "JST"   (hours  09)       "Japan Standard Time"
  , TimeZoneAbbreviationInfo "KST"   (hours  09)       "Korea Standard Time"
  , TimeZoneAbbreviationInfo "ACST"  (hours  09 + 30)  "Australian Central Standard Time"
  , TimeZoneAbbreviationInfo "ACDT"  (hours  10 + 30)  "Australian Central Daylight Time"
  , TimeZoneAbbreviationInfo "AEST"  (hours  10)       "Australian Eastern Standard Time"
  , TimeZoneAbbreviationInfo "AEDT"  (hours  11)       "Australian Eastern Daylight Time"
  , TimeZoneAbbreviationInfo "NZST"  (hours  12)       "New Zealand Standard Time"
  , TimeZoneAbbreviationInfo "NZDT"  (hours  13)       "New Zealand Daylight Time"
  ]
  where
    hours :: Int -> Offset
    hours h = Offset $ h * 60

--------------------------------------------------------------------------------
-- Common
--------------------------------------------------------------------------------

space :: TzParser Token
space = single Whitespace

spacedComma :: TzParser ()
spacedComma = choice'
  [ void $ optional' space >> punct ',' >> optional' space
  , void space
  ]

token'
  :: (Token -> Maybe a)
  -> TzParser a
token' = flip token mempty

word' :: Text -> TzParser Text
word' txt = token' $ \case
  Word w | T.toCaseFold txt == T.toCaseFold w -> pure w
  _ -> empty

relationPreposition :: TzParser Token
relationPreposition = oneOf $ map Word ["at", "on", "in"]

punct :: Char -> TzParser Token
punct c = token' $ \case
  t@(Punctuation p) | c == p -> pure t
  t@(Symbol p)      | c == p -> pure t
  _ -> empty

hourParser :: TzParser Int
hourParser = do
  (hour, len) <- numberWithLength
  guard (hour < 24 && len `elem` [1,2])
  pure hour

minuteColonParser :: TzParser Int
minuteColonParser = punct ':' >> minuteParser

minuteParser :: TzParser Int
minuteParser = do
  (min, len) <- numberWithLength
  guard (len == 2 && min < 60)
  pure min

readMinutes :: Text -> Maybe Int
readMinutes nstr = do
  min <- readMaybe $ cs nstr
  guard (T.length nstr == 2 && min < 60)
  pure min

number :: TzParser Int
number = token' $ \case
  Number nstr -> readMaybe $ cs nstr
  _ -> empty

numberWithLength :: TzParser (Int, Int)
numberWithLength = token' $ \case
  Number nstr -> (,T.length nstr) <$> readMaybe (cs nstr)
  _ -> empty

anyWord :: TzParser Text
anyWord = token' $ \case
  Word w -> pure w
  _ -> empty

anyWordT :: TzParser Token
anyWordT = token' $ \case
  w@(Word _) -> pure w
  _ -> empty

word'Hyphen'Underscore :: TzParser [Token]
word'Hyphen'Underscore = do
  let sep = oneOf $ map Punctuation ['_', '-']
  sepBy1' ((:[]) <$> anyWordT) sep

sepBy1' :: TzParser [Token] -> TzParser Token -> TzParser [Token]
sepBy1' p sep = do
  x <- p
  xs <- concat <$> many ((:) <$> sep <*> p)
  pure $ x <> xs

withStorage :: CI.CIStorage a -> Text -> TzParser a
withStorage storage key = maybe empty pure $ CI.lookup key storage

concatTokens :: [Token] -> Text
concatTokens = T.concat . map g
  where
  g :: Token -> Text
  g = \case
    Word txt -> txt
    Number txt -> txt
    Punctuation c -> T.singleton c
    Symbol c -> T.singleton c
    Whitespace -> " "
    Unknown c -> T.singleton c

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Like 'optional' but with the backtracking provided by 'try'.
-- IOW in case the given parser fails, no input is consumed.
optional' :: TzParser a -> TzParser (Maybe a)
optional' = optional . try

-- | Like 'choice' but with the backtracking provided by 'try'.
-- IOW in case any of the given parsers fails, no input is consumed.
choice' :: [TzParser a] -> TzParser a
choice' = choice . map try
