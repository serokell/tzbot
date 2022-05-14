module TzBot.Parser where

import Data.Text (Text)
import TzBot.TimeReference (TimeReference)

{- | Parses time references from an input string.

>>> parseTimeRefs "let's meet tuesday at 10am"
[TimeReference {trText = "tuesday at 10am", trTimeOfDay = 10:00:00, trDateRef = Just (DayOfWeekRef Tuesday), trLocationRef = Nothing}]

>>> parseTimeRefs "i can do it at 3pm MDT"
[TimeReference {trText = "3pm MDT", trTimeOfDay = 15:00:00, trDateRef = Nothing, trLocationRef = Just (TimeZoneAbbreviationRef "MDT")}]

>>> parseTimeRefs "how about between 2pm and 3pm?"
[ TimeReference {trText = "2pm", trTimeOfDay = 14:00:00, trDateRef = Nothing, trLocationRef = Nothing}
, TimeReference {trText = "3pm", trTimeOfDay = 15:00:00, trDateRef = Nothing, trLocationRef = Nothing}
]

-}
parseTimeRefs :: Text -> [TimeReference]
parseTimeRefs _inputStr =
  -- TODO [#1]
  []
