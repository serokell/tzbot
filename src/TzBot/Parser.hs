-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Parser where

import Universum hiding (toList, try)

import Data.Char (digitToInt)
import Data.Map qualified as M
import Data.Time (DayOfWeek(..))
import Data.Time.Calendar.Compat (DayOfMonth, MonthOfYear)
import Data.Time.LocalTime (TimeOfDay(..))
import Data.Time.Zones.All (tzNameLabelMap)
import Text.Megaparsec (Parsec, anySingle, choice, match, parseMaybe, takeRest, try)
import Text.Megaparsec.Char (char, digitChar, space, space1, string')
import Text.Megaparsec.Char.Lexer (decimal)
import TzBot.TimeReference


type TzParser = Parsec Void Text

{- | Parses time references from an input string.

>>> parseTimeRefs "let's meet tuesday at 10am"
[TimeReference {trText = "tuesday at 10am", trTimeOfDay = 10:00:00, trDateRef = Just (DayOfWeekRef Tuesday), trLocationRef = Nothing}]

>>> parseTimeRefs "i can do it at 3pm MDT"
[TimeReference {trText = "at 3pm MDT", trTimeOfDay = 15:00:00, trDateRef = Nothing, trLocationRef = Just (TimeZoneAbbreviationRef "MDT")}]

>>> parseTimeRefs "how about between 2pm and 3pm?"
[ TimeReference {trText = "2pm", trTimeOfDay = 14:00:00, trDateRef = Nothing, trLocationRef = Nothing}
, TimeReference {trText = "3pm", trTimeOfDay = 15:00:00, trDateRef = Nothing, trLocationRef = Nothing}
]

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

>>> parseTimeRefs "Let's meet between 10am and 11:30am"
[TimeReference {trText = "10am", trTimeOfDay = 10:00:00, trDateRef = Nothing, trLocationRef = Nothing},TimeReference {trText = "11:30am", trTimeOfDay = 11:30:00, trDateRef = Nothing, trLocationRef = Nothing}]

-}
parseTimeRefs :: Text -> [TimeReference]
parseTimeRefs =
  -- TODO use better error handling
  fromMaybe [] . parseMaybe timeRefsParser

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
  (newTrText, timeReference) <- match timeRefParser'
  return timeReference { trText = newTrText }

-- | Parses a single 'TimeReference', but does not collect the source text.
timeRefParser' :: TzParser TimeReference
timeRefParser' = do
  let trText = ""
  precDateRef <- optional' (do dr <- dateRefParser; space1; return dr)
  trTimeOfDay <- timeOfDayParser
  trLocationRef <- optional' $ space1 >> locRefParser
  trDateRef <- maybe (optional' $ space1 >> dateRefParser) (pure . Just) precDateRef
  pure TimeReference {..}

--------------------------------------------------------------------------------
-- TimeOfDay
--------------------------------------------------------------------------------

-- | Parses a 'TimeOfDay'.
--
-- This is permissive in the space, as it allows none to be between the time and
-- the AM/PM.
timeOfDayParser :: TzParser TimeOfDay
timeOfDayParser = do
  _ <- optional' (string' "at" >> space1)
  hour <- hourParser
  maybeMin <- optional' minuteParser
  isAm <- if isJust maybeMin
          then fromMaybe True <$> optional' isAmParser
          else isAmParser

  let todSec = 0
      todHour = if isAm then hour else hour + 12
      todMin = fromMaybe 0 maybeMin
  pure $ TimeOfDay {..}

isAmParser :: TzParser Bool
isAmParser = space >>
  (string' "AM" >> pure True) <|> (string' "PM" >> pure False)

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
  [ string' "today"     >> pure 0
  , string' "tomorrow"  >> pure 1
  , string' "yesterday" >> pure -1
  , do
      _ <- optional' (string' "in" >> space1)
      n <- decimal
      space1
      string' "days"
      pure n
  ]

dayOfWeekRefParser :: TzParser DateReference
dayOfWeekRefParser = do
  _ <- optional' (string' "on" >> space1)
  fmap DayOfWeekRef . choice' $
    [ string' "Monday"    >> pure Monday
    , string' "Tuesday"   >> pure Tuesday
    , string' "Wednesday" >> pure Wednesday
    , string' "Thursday"  >> pure Thursday
    , string' "Friday"    >> pure Friday
    , string' "Saturday"  >> pure Saturday
    , string' "Sunday"    >> pure Sunday
    ]

dayOfMonthRefParser :: TzParser DateReference
dayOfMonthRefParser = do
  _ <- optional' (string' "on" >> space1)
  choice' $
    [ do
        _ <- optional' (string' "the" >> space1)
        dayOfMonth <- dayOfMonthParser
        _ <- optional' (space1 >> string' "of")
        monthOfYear <- optional' (space1 >> monthOfYearParser)
        pure $ DayOfMonthRef dayOfMonth monthOfYear
    , do
        monthOfYear <- Just <$> monthOfYearParser
        space1
        optional' (string' "the" >> space1)
        dayOfMonth <- dayOfMonthParser
        pure $ DayOfMonthRef dayOfMonth monthOfYear
    ]

dayOfMonthParser :: TzParser DayOfMonth
dayOfMonthParser = do
  n <- decimal
  optional' (string' "st" <|> string' "nd" <|> string' "rd" <|> string' "th")
  if n < 32 then pure n else empty

monthOfYearParser :: TzParser MonthOfYear
monthOfYearParser = choice' $
  [ string' "January"   >> pure 1
  , string' "February"  >> pure 2
  , string' "March"     >> pure 3
  , string' "April"     >> pure 4
  , string' "May"       >> pure 5
  , string' "June"      >> pure 6
  , string' "July"      >> pure 7
  , string' "August"    >> pure 8
  , string' "September" >> pure 9
  , string' "October"   >> pure 10
  , string' "November"  >> pure 11
  , string' "December"  >> pure 12
  ]

--------------------------------------------------------------------------------
-- LocationReference
--------------------------------------------------------------------------------


locRefParser :: TzParser LocationReference
locRefParser = do
  _ <- optional' (string' "in" >> space1)
  choice [offsetRefParser, tzAbbrRefParser, tzRefParser]

tzRefParser :: TzParser LocationReference
tzRefParser =
  fmap TimeZoneRef . choice' .
    map (\(str, tzLabel) -> string' (decodeUtf8 str) >> pure tzLabel) $
      M.toList tzNameLabelMap

offsetRefParser :: TzParser LocationReference
offsetRefParser = do
  string' "UTC+"
  hours <- hourParser
  maybeMins <- optional' minuteParser
  let minutesTotal = 60 * hours + fromMaybe 0 maybeMins
  pure . OffsetRef $ Offset minutesTotal

tzAbbrRefParser :: TzParser LocationReference
tzAbbrRefParser =
  fmap TimeZoneAbbreviationRef . choice' $
    map ((\tzAbbr -> string' (unTimeZoneAbbreviation tzAbbr) >> pure tzAbbr) . tzaiAbbreviation)
      knownTimeZoneAbbreviations

--------------------------------------------------------------------------------
-- Common
--------------------------------------------------------------------------------

hourParser :: TzParser Int
hourParser = do
  r <- digitToInt <$> digitChar
  maybeL <- fmap digitToInt <$> optional' digitChar
  let n = maybe r (\l -> r * 10 + l) maybeL
  if n < 24 then pure n else empty

minuteParser :: TzParser Int
minuteParser = do
  char ':'
  r <- digitToInt <$> digitChar
  l <- digitToInt <$> digitChar
  let n = r * 10 + l
  if n < 60 then pure n else empty

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
