# Edge cases & pitfalls

<!-- TOC -->

- [Edge cases & pitfalls](#edge-cases--pitfalls)
  - [time library](#time-library)
  - [Ambiguous times](#ambiguous-times)
  - [Invalid times](#invalid-times)

<!-- /TOC -->

## `time` library

`TimeZone` from the `time` library is a [misnomer](https://mail.haskell.org/pipermail/haskell-cafe/2014-March/113364.html).
It does NOT represent a timezone, which can observe different offsets at different times, like `Europe/London`.
It represents a static "offset" with an optional name, like `UTC+01:00 (BST)`.

To help make this distinction a bit more clear, we have this type alias:

```hs
type NamedOffset = TimeZone
```

In fact, the `time` library does not support timezones at all.

Use the `TZ` and `TZLabel` data types from the `tz` library to represent timezones.

---

The choice of the name for `LocalTime` can be confusing.
It's meant to represent a time whose timezone/offset are unknown.

---

Given what we now know about the `TimeZone` type, one could argue `ZonedTime` (a `LocalTime` + `NamedOffset`) is not that useful.
To unambiguously (see "Ambiguous times" below) represent a time in a given timezone,
one should use `TZ` + `LocalTime` + `NamedOffset`.

## Ambiguous times

At 01:59:59 on 2022-11-06, the America/Winnipeg timezone switches from CDT (UTC-5) to CST (UTC-6).
In other words, the clock “goes backwards” 1 hour, back to 01:00:00.

This means that, on that day, on that timezone, the time 01:30:00 will happen twice.

Thus, if, on 2022-11-05, a user says "I'm a night owl, so let's meet at 01:30am", that'll be ambiguous.
They could be referring to 01:30am CDT or 01:30am CST.

```hs
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import qualified Data.Time.Zones as TZ
import qualified Data.Time.Zones.All as TZ

λ> iso8601ParseM @_ @LocalTime "2022-11-06T01:30:00" <&> TZ.localTimeToUTCFull (TZ.tzByLabel TZ.America__Winnipeg)
LTUAmbiguous
  { _ltuFirst = 2022-11-06 06:30:00 UTC
  , _ltuSecond = 2022-11-06 07:30:00 UTC
  , _ltuFirstZone = CDT
  , _ltuSecondZone = CST
  }
```

When this happens, we should send an ephemeral message to the sender and
ask them to edit the message and specify an offset / timezone abbreviation.

## Invalid times

At 01:59:59 on 2022-03-13, the America/Winnipeg timezone switches from CST (UTC-6) to CDT (UTC-5).
In other words, the clocks “skip” 1 hour, straight to 03:00:00.

Thus, if, on 2022-03-12, a user says “I'm a night owl, so let's meet at 02:30am”, that reference is invalid.
Notice how `localTimeToUTCFull` returns `LTUNone` instead of `LTUUnique`:

```hs
λ> iso8601ParseM @_ @LocalTime "2022-03-13T02:30:00" <&> TZ.localTimeToUTCFull (TZ.tzByLabel TZ.America__Winnipeg)
LTUNone
  { _ltuResult = 2022-03-13 08:30:00 UTC
  , _ltuZone = CST
  }
```

In this case, `_ltuResult` represents the given time (i.e. "02:30") shifted
forward by the duration of the "time gap" ("03:30 American/Winnipeg").

When this happens, we should send an ephemeral message to the sender informing
them that their timezone's offset will change around that time.
