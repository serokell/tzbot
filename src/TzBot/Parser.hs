-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Parser
  ( parseTimeRefs
  ) where

import Universum hiding (many, toList, try)

import Data.Map qualified as M
import Data.String.Conversions (cs)
import Data.Text qualified as T
import Data.Time (DayOfWeek(..))
import Data.Time.Calendar.Compat (DayOfMonth, MonthOfYear)
import Data.Time.LocalTime (TimeOfDay(..))
import Data.Time.Zones.All (TZLabel, tzNameLabelMap)
import Glider.NLP.Tokenizer (Token(..), tokenize)
import Text.Megaparsec hiding (Token)

import TzBot.Instances ()
import TzBot.TimeReference
import TzBot.Util qualified as CI

type TzParser = Parsec Void [Token]

{- | Parses time references from an input string.

>>> parseTimeRefs "let's meet tuesday at 10am"
[TimeReference {trText = "tuesday at 10am", trTimeOfDay = 10:00:00, trDateRef = Just (DayOfWeekRef Tuesday), trLocationRef = Nothing}]

>>> parseTimeRefs "i can do it at 3pm MDT"
[TimeReference {trText = "at 3pm MDT", trTimeOfDay = 15:00:00, trDateRef = Nothing, trLocationRef = Just (TimeZoneAbbreviationRef "MDT")}]

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
[TimeReference {trText = "at 11am on the 4th of April", trTimeOfDay = 11:00:00, trDateRef = Just (DayOfMonthRef 4 (Just 4)), trLocationRef = Nothing}]

>>> parseTimeRefs "at 11am on April 4"
[TimeReference {trText = "at 11am on April 4", trTimeOfDay = 11:00:00, trDateRef = Just (DayOfMonthRef 4 (Just 4)), trLocationRef = Nothing}]

>>> parseTimeRefs "at 11am on 4 April"
[TimeReference {trText = "at 11am on 4 April", trTimeOfDay = 11:00:00, trDateRef = Just (DayOfMonthRef 4 (Just 4)), trLocationRef = Nothing}]

>>> parseTimeRefs "9am in europe/london"
[TimeReference {trText = "9am in europe/london", trTimeOfDay = 09:00:00, trDateRef = Nothing, trLocationRef = Just (TimeZoneRef Europe__London)}]

>>> parseTimeRefs "2pm CST"
[TimeReference {trText = "2pm CST", trTimeOfDay = 14:00:00, trDateRef = Nothing, trLocationRef = Just (TimeZoneAbbreviationRef "CST")}]

>>> parseTimeRefs "10am UTC+03:00"
[TimeReference {trText = "10am UTC+03:00", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Just (OffsetRef 180)}]

>>> parseTimeRefs "10am UTC+3"
[TimeReference {trText = "10am UTC+3", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Just (OffsetRef 180)}]

>>> parseTimeRefs "10am UTC-3"
[TimeReference {trText = "10am UTC-3", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Just (OffsetRef (-180))}]

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
[TimeReference {trText = "13:00 Nov 06", trTimeOfDay = 13:00:00, trDateRef = Just (DayOfMonthRef 6 (Just 11)), trLocationRef = Nothing}]

>>> parseTimeRefs "11:12:13 nOv 1"
[TimeReference {trText = "11:12:13 nOv 1", trTimeOfDay = 11:12:00, trDateRef = Just (DayOfMonthRef 1 (Just 11)), trLocationRef = Nothing}]

>>> parseTimeRefs "13:00 06 Nov"
[TimeReference {trText = "13:00 06 Nov", trTimeOfDay = 13:00:00, trDateRef = Just (DayOfMonthRef 6 (Just 11)), trLocationRef = Nothing}]

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
[TimeReference {trText = "13:00 06 nov", trTimeOfDay = 13:00:00, trDateRef = Just (DayOfMonthRef 6 (Just 11)), trLocationRef = Nothing}]

>>> parseTimeRefs "13:00 nov 06"
[TimeReference {trText = "13:00 nov 06", trTimeOfDay = 13:00:00, trDateRef = Just (DayOfMonthRef 6 (Just 11)), trLocationRef = Nothing}]

>>> parseTimeRefs "today,10am"
[TimeReference {trText = "today,10am", trTimeOfDay = 10:00:00, trDateRef = Just (DaysFromToday 0), trLocationRef = Nothing}]

>>> parseTimeRefs "today ,   10am"
[TimeReference {trText = "today , 10am", trTimeOfDay = 10:00:00, trDateRef = Just (DaysFromToday 0), trLocationRef = Nothing}]

>>> parseTimeRefs "today,   10am"
[TimeReference {trText = "today, 10am", trTimeOfDay = 10:00:00, trDateRef = Just (DaysFromToday 0), trLocationRef = Nothing}]

>>> parseTimeRefs "today ,10am"
[TimeReference {trText = "today ,10am", trTimeOfDay = 10:00:00, trDateRef = Just (DaysFromToday 0), trLocationRef = Nothing}]

>>> parseTimeRefs "10am MSKC"
[TimeReference {trText = "10am", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Nothing}]

>>> parseTimeRefs "10am aMeRiCa/Argentina/Buenos_Aires"
[TimeReference {trText = "10am aMeRiCa/Argentina/Buenos_Aires", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Just (TimeZoneRef America__Argentina__Buenos_Aires)}]

>>> parseTimeRefs "10am America/North_Dakota/New_Salem"
[TimeReference {trText = "10am America/North_Dakota/New_Salem", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Just (TimeZoneRef America__North_Dakota__New_Salem)}]

>>> parseTimeRefs "10am America/port-au-Prince"
[TimeReference {trText = "10am America/port-au-Prince", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Just (TimeZoneRef America__Port_au_Prince)}]

-}
parseTimeRefs :: Text -> [TimeReference]
parseTimeRefs =
  -- TODO use better error handling
  fromMaybe []
    . parseMaybe timeRefsParser
    -- time reference can be either at the beginning or after the space
    . (Whitespace :)
    . tokenize

-- | Parser for multiple 'TimeReference' s.
--
-- This looks for all of them in the input and ignores everything surrounding.
timeRefsParser :: TzParser [TimeReference]
timeRefsParser = choice'
  [ do
      tr <- try timeRefParser
      trs <- timeRefsParser
      return $ tr : trs
  , anySingle >> timeRefsParser
  , takeRest >> pure []
  ]

-- | Parses a single 'TimeReference', consuming all input.
timeRefParser :: TzParser TimeReference
timeRefParser = do
  _ <- space
  (newTrText, timeReference) <- match timeRefParser'
  return timeReference { trText = concatTokens newTrText }

-- | Parses a single 'TimeReference', but does not collect the source text.
timeRefParser' :: TzParser TimeReference
timeRefParser' = do
  let trText = ""
  precDateRef <- optional' (do dr <- dateRefParser; spacedComma; return dr)
  trTimeOfDay <- timeOfDayParser
  trLocationRef <- optional' $ spacedComma >> locRefParser
  trDateRef <- maybe (optional' $ spacedComma >> dateRefParser) (pure . Just) precDateRef
  pure TimeReference {..}

-- | Parses a 'TimeOfDay'.
--
-- This is permissive in the space, as it allows none to be between the time and
-- the AM/PM.
timeOfDayParser :: TzParser TimeOfDay
timeOfDayParser = do
  _ <- optional' (relationPreposition >> space)
  hour <- hourParser
  maybeMin <- optional' minuteParser
  let secondParser = minuteParser
  _ <- optional' secondParser

  isAm <- if isJust maybeMin
          then fromMaybe True <$> optional' isAmParser
          else isAmParser

  let todSec = 0
      todHour
        | isAm = hour
        -- pm here
        | hour < 12 = hour + 12
        -- ignore pm if hour > 12
        | otherwise = hour
      todMin = fromMaybe 0 maybeMin
  pure $ TimeOfDay {..}

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
  , dayOfMonthRefParser
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

dayOfMonthRefParser :: TzParser DateReference
dayOfMonthRefParser = do
  _ <- optional' (relationPreposition >> space)
  choice' $
    [ do
        _ <- optional' (word' "the" >> space)
        (dayOfMonth, hasSuffix) <- dayOfMonthParser
        _ <- optional' (space >> word' "of")
        monthOfYear <- optional' (space >> monthOfYearParser)
        -- otherwise we can consider an unrelated number
        -- as a valid day of month
        guard $ hasSuffix || isJust monthOfYear
        pure $ DayOfMonthRef dayOfMonth monthOfYear
    , do
        monthOfYear <- Just <$> monthOfYearParser
        spacedComma
        optional' (word' "the" >> space)
        dayOfMonth <- fst <$> dayOfMonthParser
        pure $ DayOfMonthRef dayOfMonth monthOfYear
    ]

dayOfMonthParser :: TzParser (DayOfMonth, Bool)
dayOfMonthParser = do
  n <- number
  guard (n < 32)
  hasSuffix <- isJust <$>
    optional' (word' "st" <|> word' "nd" <|> word' "rd" <|> word' "th")
  pure (n, hasSuffix)

monthOfYearParser :: TzParser MonthOfYear
monthOfYearParser = do
  monName <- anyWord
  withStorage monthStorage monName

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
  (sign :: Int) <- choice
    [ punct '+' >> pure 1
    , punct '-' >> pure (-1)
    ]
  hours <- hourParser
  maybeMins <- optional' minuteParser
  let minutesTotal = 60 * hours + fromMaybe 0 maybeMins
  pure . OffsetRef $ Offset $ sign * minutesTotal

tzAbbrRefParser :: TzParser LocationReference
tzAbbrRefParser = do
  abbr <- anyWord
  res <- TimeZoneAbbreviationRef . tzaiAbbreviation <$>
    withStorage knownTimeZoneAbbreviations abbr
  notFollowedBy $ choice' $ map punct ['+', '-']
  pure res

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

minuteParser :: TzParser Int
minuteParser = do
  punct ':'
  (min, len) <- numberWithLength
  guard (len == 2 && min < 60)
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
