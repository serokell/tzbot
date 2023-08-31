<!--
-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0
-->

# Implementation details

The bot's main function is to capture time references in a message and convert them
to the receiver's time zone. This can be triggered by some events:
  * A new message was posted in a channel where the bot is present;
  * A message has been edited and it now contains some new time references;
  * The user triggered an entrypoint from the message's context menu `⋮`;
  * The user DMed the bot.

When a new event with an associated message has arrived, it is processed according to the following steps:
  * The structure of the message is analyzed. The text is divided into separate pieces
    that can contain whole time references. Code blocks are ignored.
  * Text chunks are parsed (see `TzBot.Parser` module) into `TimeReference` structures.
  * The resulting `TimeReference` value is turned into a `TimeReferenceToUtcResult`,
    a timestamp with some auxiliary information; see `TzBot.TimeReference`.
    This module handles these issues:
    * When the user does not explicitly mention a date, it attempts to guess which date the user meant.
    * When the user does not explicitly mention a `LocationReference`,
      it infers the user must have been referring to the timezone they are in.
    * Convert the `LocalTime` from the given `LocationReference` to UTC.
    * If the `LocalTime` is invalid or ambiguous, an error is returned.
  * The `TimeReferenceToUtcResult` is then rendered either as Slack blocks or as just plain text.
    The output contains the original time reference; if the `DateReference` or `LocationReference` we inferred, they will be appended.

There are two main kinds of Slack interactions: listening for Slack events and querying Slack APIs.
The former is implemented using the `slacker` package, the latter is implemented
via the `servant` library and used for posting messages, opening
and updating modals and fetching relevant information about users, channels, and messages.
This information is cached in order to maximally reduce the number of API calls and pass Slack's rate limit constraints.

## Parser

Given a text chunk, it is firstly tokenized; this is performed in order to filter possible
time references that are accidentally incorporated into unrelated pieces of text (like "foo35pmkek"
that can be parsed as "5pm"). The tokenizer used currently (from the `glider-nlp` package) is not perfect
and may be replaced with a better alternative later.

After this, the parser goes through the resulting tokens and tries to collect all the time
references remembering their original text.
Each time reference must contain a time of day.
It can also contain an optional location reference and date reference.

The following formats of time are supported:
* `7pm`
* `7 PM`
* `7.30pm`
* `7:30`
* `7:30pm`
* `19h`
* `19h30`

We also do our best effort to parse more complex sentence structures, such as:
* Time intervals with a shared `am`/`pm` qualifier:
  * `7:00 - 10:00 PM`:
    "PM" applies to both "7:00" and "10:00".
* Multiple time references with a shared date/location reference:
  * `Let's go on Wednesday at 10:00 or 11:00`:
    "Wednesday" applies to both "10:00" and "11:00"
  * `How about Wednesday at 10:00 / 11:00 OR Thursday at 14:00 / 15:00 UTC`:
    "Wednesday" applies to "10:00" and "11:00", "Thursday" applies to "14:00" and "15:00".
    "UTC" applies to all 4 time references.

By default, the parser will not recognize strings such as `7` and `7.30`.
We can't be sure the user meant to refer to a time of day, they may have meant `7.30 euros`.
We can only conclusively say `7.30` refers to "7 hours and 30 minutes" if:
* An `am`/`pm` qualifier is used explicitly, e.g. "Let's meet at 7.30 am"
* An `am`/`pm` qualifier can be inferred from the context, e.g. "Let's meet between 7 and 8pm"

Sometimes messages can have a rather tricky structure and the date/location reference can be distant from the time reference:
  > Tomorrow, I'll be able to connect between 10am and 11am

This issue seems to require analyzing sentence structure and is quite subtle and interesting; we hope
to solve it one day :smile:

## Time conversion

The main goal of the `TzBot.TimeReference` module is to try to convert the obtained `TimeReference` to `UTCTime`
collecting some additional meta info and handling errors related to clock changes.
Sometimes the user provides incomplete information, and we have to infer what they meant.
* "1am": The user supplied the time, but not the date or the time zone.
  * In this case, we interpret "1am" as 1am of the current day; unless that time is already in the past,
    in which case we infer they must mean 1am the next day.
  * When the time zone is omitted, we infer that the reference must be relative to the sender's own time zone.
* "1am on Wednesday": The user supplied the time and day of the week, but they didn't specify which week.
  * This is interpreted as 1am of the current day if today is Wednesday.
    If it's not, then we infer it must mean 1am the next Wednesday.
* "1am on the 12th": The user supplied the time and day of the month, but not the month or year.
  * See `chooseBestMonth` and `chooseBestYear`.
* "1am on March 12th": The user supplied the time, day of the month, and month, but not the year.
  * See `chooseBestYear`.

If our inference does not match the sender's true intention, and if the time zone rules for the day
we inferred differ from the rules for the day they meant, then there is a possibility that the conversion will
not be correct.

## Clock change warnings

When the user does not specify a date and we have to infer it, sometimes this inferred date may be
close to a clock change (a time when the clocks go forward or backward).
When this happens, an incorrect guess on our part is very likely to result in a misleading conversion.

For example, say it's 01:00 AM, 25 Mar 2023, and the user is in Europe/London.
At this point in time, this time zone observes UTC+0.

If they send a message containing "10am", we'll infer they mean "10am today, 25 March".
If a reader is in Europe/Moscow (which observes UTC+3 all year round), we'll convert "10am" to "13:00, 25 March 2023, in Europe/Moscow".

However, if our inference is wrong, and they actually meant "10am tomorrow", then this conversion is completely wrong.
On the morning of the 26th, at 01:00, Europe/London [transitions from UTC+0 to UTC+1](https://www.timeanddate.com/time/change/uk/london?year=2023),
which means the result of the conversion _should_ have been "12:00, 26 March 2023, in Europe/Moscow", not "13:00".

For this reason, when we infer a date, and if that date is "near" (3 days before or after) a clock change,
we warn the user that the accuracy of the conversion depends on whether we inferred the date correctly or not.

> "10am", 25 March 2023 in Europe/London

> 13:00, Saturday, 25 March 2023 in Europe/Moscow

> _Warning: We inferred that "10am" refers to 25 March 2023 in Europe/London and converted it to Europe/Moscow, but there is a time change near this date_:
>
>   • _At 01:00, 26 March 2023 in Europe/London, the clocks are turned forward 1 hour(s)_.
>
> _Beware that if this inference is not correct and the sender meant a different date, the conversion may not be accurate._

Note that we check if there is a clock change "near" the inferred date in both the "source" time zone and in each receiver's time zone.
If there is a clock change in the source time zone, every channel member will see a warning.
If there is a clock change in a receiver's time zone, only that receiver will see a warning.

## Processing messages from channels and direct messages

Every time a user sends a message, the bot will process it and, if it contains time references,
send ephemeral messages to channel members.

When the sender of a message explicitly refers to a timezone different than the one they are in,
they will also receive an ephemeral message themselves.
For example, if the sender lives in Helsinki and types "9am UTC", they will
receive an ephemeral message with "11am in Europe/Helsinki".

Other channel members will always receive an ephemeral message, even if they are in the same
timezone as the sender.
The decision is controversial, but we decided to do that because the receiver may not be aware they are in the same timezone as the sender, and the absence of an ephemeral message may
interpreted as "why isn't the bot working?".

Users can also DM the bot. In this case, the bot will return an ephemeral message just as other
users would see it if the message was posted in an ordinary channel.

AFAIU there is no way to add the bot to an existing DM
(see [this discussion](https://forums.slackcommunity.com/s/question/0D53a00008vsItQCAU)),
so messages in such chats can be only converted using Slack entrypoints.

## Using entrypoints

Currently there are two supported entrypoints in the message context menu `⋮`:

* _Convert time references_: Opens a modal window with all of the message's time references
  and the corresponding time converted to the user's time zone.
  The real advantage of this entrypoint is that it can be used for converting:
    * Old messages for which all ephemerals have been erased.
    * Messages that are not visible to the bot by default (e.g. from direct messages).
* _Report an issue_: Opens a modal window where the user can leave any feedback. The feedback is stored in the file given by the `feedbackFile` config var / `SLACK_TZ_FEEDBACK_FILE` env var.

See [this comprehensive guide](https://api.slack.com/surfaces/modals/using) for details on working with modals
and handling user events.

## Handling message editing

When a message containing time references is edited and, as a result, it now contains
new time references, we send a new batch of ephemeral messages.

To preserve context, this new ephemeral message will contain a permalink to the user's message.
The user can either hover over the permalink and a preview will be shown,
or click it and Slack will scroll up to the user's message.

## Ignoring code blocks

The main reason for this is the fact that logs are often put into such blocks, and logs contain a lot of
timestamps, which can overload the bot server. Also, code blocks often contain artificial character sequences
that can be wrongly recognized as time references, and we also want to avoid that.

This feature is implemented by analyzing the block structure of the incoming message
(see `TzBot.Slack.API.MessageBlock`). AFAIU, it is not properly documented, the only link that describes
what's going on is [this API update report](https://api.slack.com/changelog/2019-09-what-they-see-is-what-you-get-and-more-and-less).
As the solution can't be considered stable, if we fail to parse Slack's block structure,
we fall back to a naive search for ``` and \` (see `ignoreCodeBlocksManually`).

## Caching data

The bot often needs some data that changes relatively rarely:
* users' profile info;
* conversation members;
* message details.

This information is cached using the logic implemented in `TzBot.Cache` with the aid of the `cache` package.

To avoid having many cache entries expiring at once (which would lead
to the bot sending many Slack API `users.info` requests, possibly going over the rate limit),
we randomize the cache expiry times.

Since cache entries are not automatically deleted after they expire,
a separate thread is running to purge all expired items from time to time.
