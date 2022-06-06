<!--
-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0
-->

# tzbot

| This project is participating in [ZuriHac 2022](https://zfoh.ch/zurihac2022/projects.html)! Join the [discord server](https://discord.gg/G49DeVY) for communications. |
| --- |

`tzbot` is a Slack bot that detects messages with references to some point in time,
and converts them to your timezone.

When a user in the Europe/Riga timezone sends a message such as:

> Hey, can we meet tomorrow at 6pm?

Every user on a timezone different than the sender's will receive an
"ephemeral message" (a message visible only to that user):

> "tomorrow at 6pm" in Europe/Riga will be 10:00, 12 April 2022 in America/Winnipeg

## Features

* [ ] When a Slack message is edited, the bot sends a new ephemeral message with the updated times.
* [ ] Time references in codeblocks are ignored.
* [ ] Supports messages sent to public and private channels, shared channels,
      direct messages, group direct messages, and replies in threads.
* [ ] Detects references to:
  * invalid times (e.g., "tomorrow at 1:30", but the clocks skip from 00:59 to 02:00 on that day).
  * ambiguous times (e.g. "tomorrow at 1:30", but the clocks are set back from 01:59
    to 01:00 on that day, meaning the time "1:30" will occur twice at two different offsets).
* [ ] Handles references with timezones, offsets and _some_ timezone abbreviations
  (see [timezone_abbreviations.md](docs/timezone_abbreviations.md) for a full list).
    > tuesday at 11am Australia/Brisbane
    > tuesday at 11am UTC+3
    > tuesday at 11am CST

For the initial MVP, we'll only support messages sent to public/private channels.

## Contributing

See:
  * [Contributing](CONTRIBUTING.md)
  * [Development](docs/development.md)
  * [Edge cases & pitfalls](docs/pitfalls.md)


## Comparison with similar tools

* Timely is very similar to `tzbot`, but it:
  * Reacts everytime someone says the word "now", which can be a bit annoying when you say e.g.
    "I just merged my PR, we can _now_ use this feature".
  * Reacts to timestamps in codeblocks.
    This means that when you post some application's logs in Slack, you get flooded with notifications.
  * Does not react when a user edits their message.
  * Does not understand references that include a timezone name or an offset.
* Timezone Butler:
  * Assumes all references are relative to the current day, i.e., it doesn't distinguish
    "10am" from "10am tomorrow", which can have an impact when DST changes occur.
  * It understands "10am" but not "10 am".
  * It incorrectly interprets "3:30" as "3:30pm", instead of "3:30am".
  * Does not react when a user edits their message.
  * Does not understand references that include a timezone name or an offset.
