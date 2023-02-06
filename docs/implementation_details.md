<!--
-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0
-->

# Implementation details

The main bot's function is capturing time references in message's text and converting them
to the receiver's time zone. This can be triggered by some events:
  * A new message was posted in a channel where the bot is present;
  * The message that was previously translated was recently edited and some new time references
    appeared;
  * User triggered an entrypoint from the message's context menu;
  * User DMed the bot.

When a new event with an associated message has come, it is processed with such common steps:
  * Structure of the message that is provided by Slack is analyzed: according to this structure
    message text is divided into separate pieces that can contain whole time references;
    code blocks are ignored.
  * Text chunks are parsed (see `TzBot.Parser` module) into `TimeReference` structure.
  * Resulting `TimeReference` value is turned into a timestamp with some auxiliary information
    (`TimeReferenceToUtcResult` datatype); see `TzBot.TimeReference`.
    This module handles next issues:
    * When the exact date can't be defined from the original time reference, it attempts to guess
      the meant date.
    * Calculate final local time in the original timezone/offset and convert it to UTC timestamp.
    * If the resulting localtime is invalid or ambiguous because of time shifts, corresponding error
      is returned.
  * `TimeReferenceToUtcResult` is then rendered either as Slack blocks or as just plain text, resulting
    output contains original time reference appended with inferred parts that are not present in the
    original time reference.

This pure part is wrapped with IO layer which handles interactions with Slack, logging etc.
This layer handles all the events that are listed at the beginning of this section.
There are two main kinds of Slack interactions: listening for Slack events and querying Slack APIs.
The former is implemented over `slacker` package, the latter is implemented
via `servant` library and used for posting messages, opening
and updating modals and fetching relevant information about users, channels, messages.
This information is cached in order to maximally reduce the number of API calls and pass the
rate limit constraints that are checked by Slack servers.

## Parser

Given a text chunk, they are firstly tokenized; this is performed in order to filter possible
time references that are accidentally incorporated into unrelated pieces of text (like "foo35pmkek"
that can be parsed as "5pm"). The tokenizer used currently (from `glider-nlp` package) is not perfect
and may be replaced with a better alternative later.

After this, the parser goes through the resulting tokens and tries to collect all time references remembering
their original text. The core element of a time reference is time of day, it must be present
in each one; some other optional elements can also be contained: location reference, date reference.
The ordering of all these elements can be arbitrary but each element can be found only once per time reference.

Following formats of time are supported:
* `7pm`
* `7 PM`
* `7.30pm`
* `7:30pm`
* `7:30`
* `19h`
* `19h30`

`7.30` is not supported intentionally because it rather represents a fractional number than time.

Current parser implementation is mainly focused on wide variety of different correct
variations and doesn't concern about possible grammatically incorrect, but successfully
parsed combinations.

Sometimes message text can have rather tricky structure when date reference can be away from the time itself:
  > Tomorrow, I'll be able to connect between 10am and 11am

This issue seems to require analyzing sentence structure and is quite subtle and interesting; we hope
to solve it one day :smile:

## Time translation

Main goal of the `TzBot.TimeReference` module is to try to translate the obtained `TimeReference` to the UTCTime
collecting some additional meta info and handling possible timeshift errors. If date reference is not total, we
try to guess what date was really meant (e.g. `chooseBestMonth` and `chooseBestYear`). In situations when exact date
can't be defined, the real mentioned date can be slighly different and if a timeshift in sender/receiver's timezone
is somewhere around then time conversion correctness can depend on the exact date. The translation module tries
to pick some reasonable interval around the resulting time and check it for timeshifts. If one found, the user
is warned about it.

## Processing messages from channels and direct messages

Most probable type of the bot work is processing channel messages and mailing them to the channel members.
Ephemeral that is sent to the author contains either time reference errors or translated time reference
that are _not_ in the same timezone as author's. This will help the sender to check that he hasn't mistaken
when translating his time to that timezone; i.e. if the sender lives in Helsinki, "9am UTC" will be returned
to him as "11am in Europe/Helsinki". If there are no such time refs and no errors, the author will receive
no ephemeral message at all. Other users will get all time translations, possibly as "your timezone is the same"
for those with matching timezones.

We could just not send anything to users that have the same timezone as the reference has, but decided to
just note that this time reference doesn't need to be translated. This is because absence of the translation
can be caused by several reasons:
* the bot doesn't work at all;
* it failed to parse this time;
* it wasn't able to send an ephemeral for some reason, etc.
By saying "your timezone is the same", we just say "everything is OK, but just see the original
time ref, it's correct for you".

Users can also DM the bot, in this case the bot will return him an ephemeral message such as other
users would see it if the message was posted in an ordinary channel.

AFAIU there is no way to add the bot to an existing DM, so messages in such chats can be only translated
using Slack entrypoints.

## Using entrypoints

Currently there are two supported entrypoints in the message context menu: _Translate_ and _Report_.
The first one works just as expected:
it renders message text and translation pairs separately. The real advantage of this entrypoint is that it can
be triggered both for old messages for which all ephemerals are gone and for messages that are not visible
for the bot by default (e.g. from direct message chats) The _Translate_ modal has "Report" button that leads
to the _Report_ modal that is also reachable through mentioned _Report_ endpoint. There user can leave any
feedback about the bot working wrongly.

See [this comprehensive guide](https://api.slack.com/surfaces/modals/using) for details of working with modals
and handling user events. In the `tzbot`, values that are shared between modals and button events are
listed in the `Slack.Fixtures` module.

## Handling message editing

The main concern here is to send an ephemeral such that it was easy to return to the original message's context.
When the message was edited just right after it was posted, it's not a problem because the new ephemeral
will not be too far. But if it's not the case, the ephemeral will turn out to appear "in the open field" and
it may be hard to understand what this ephemeral refers to. The solution is to send ordinary ephemerals but
put a message permalink to the edited message in its head. This is quite convenient, because the user
can get the original text by hovering over that link, and time conversions will then be close to the text.
If the original text was too long and it can be viewed in the pop-up, the user can just click the link and
return to the message, possibly using entrypoints afterwards.

## Ignoring code blocks

The main reason for this is the fact that logs are often put into such blocks, and logs contain a lot of
timestamps, which can overload the bot server. Also code blocks often contain artificial character sequences
that can be wrongly recognized as time references, and we also want to avoid that.

This feature is implemented by analyzing Slack provided block structure of the incoming message
(see `TzBot.Slack.API.MessageBlock`). AFAIU it is not properly documented, the only link that describes
what's going on is [this API update report](https://api.slack.com/changelog/2019-09-what-they-see-is-what-you-get-and-more-and-less).
As the solution can't be considered stable, a reserve option is also added that tries to analyze code blocks manually.

## Caching data

For work the bot needs some data that changes relatively rarely:
* user info;
* conversation members;
* message details.
For this small cache wrapper over `cache` instance was implemented in `TzBot.Cache`.
Additionally, it supports expiry time randomization; it was initially introduced to avoid
multiple `user.info` calls at once. Also, as items are not automatically deleted after they
get expired, a separate thread is running, purging all expired items form time to time.
