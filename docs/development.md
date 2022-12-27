<!--
-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0
-->

# Development

## Glossary

In this codebase and accompanying documentation, we may use the following terms:

* **Timezone**:
  "A time zone is a region on Earth that has a uniform, legally mandated standard time".
  A timezone contains a set of rules dictating which offset(s) is/are observed throughout the year.
  For example: *under current law*, the "Europe/London" timezone observes the offset BST (UTC+01:00) during
  summer and the offset GMT (UTC+00:00) otherwise.
  These rules change over time, by governmental decree.
  [IANA](http://www.iana.org/time-zones) maintains a database of timezones and their rules.
  We can access this via the [`tz`][tzlabel] package on Hackage.
* **Timezone abbreviations**:
  We tend to informally call `GMT`, `BST` and `CST` "timezones".
  However, timezones they are not.
  They represent a static *offset* (+00:00, +01:00 and -06:00, respectively)!
  A timezone may observe one or more offsets throughout the year.
  In the literature, these are usually called "timezone abbreviations"
  (sidenote: one could argue "offset abbreviations" or "named offsets" would be more accurate names).
  See: [Time Zones Aren’t Offsets – Offsets Aren’t Time Zones][time-zones-offsets].
* **Sender**:
  A user who sends a slack message with a reference to some moment in time.
* **[Ephemeral message](https://api.slack.com/messaging/managing#ephemeral)**:
  A Slack message sent from a Slack bot visible only to a specific user.
  It has a "Only visible to you" label next to it.
  The docs say "they do not persist across reloads, between desktop and mobile apps, or across sessions".
  In practice, they do seem to persist in the Slack desktop client but not in the Android client.

## Architecture

Slack supports [these APIs](https://api.slack.com/apis/connections):

* Querying data:
    * [Web API](https://api.slack.com/web):
      *send* HTTP requests to Slack (e.g. to query which timezone a user is in).
* Receiving events:
    * [Events API](https://api.slack.com/apis/connections/events-api):
      *receive* an HTTP request from Slack notifying you of some event.
    * [Socket mode](https://api.slack.com/apis/connections/socket):
      receive events via a websocket connection.
    * RTM (Real Time Messaging) API:
      receive events *and* post slack messages via a websocket connection.
      Deprecated, new apps can’t use this mode.

At the moment, `tzbot` is setup to receive events via Socket mode (using [`slacker`](https://github.com/velveteer/slacker)),
and query data via the Web API (see the `TzBot.Slack.*` modules).

Since "apps using Socket Mode are not currently allowed in the public Slack App Directory”,
we'll also add support for receiving events via the Events API in the future.

## Makefile

We have a [Makefile](/Makefile) which provides shortcuts for the most
common developers' activities.

## Setting up a dev environment

**NOTE**: Most [issues](https://github.com/serokell/tzbot/issues) (except #3)
do not require setting up a Slack workspace. They can be worked on in isolation.

1. [Create a Slack Workspace](https://slack.com/get-started#/createnew)
1. Create a new Slack App
    * Go to <https://api.slack.com/apps> and pick "Create New App" > "From an app manifest"
      and paste the contents of the [`manifest.yml`](../manifest.yml) at the root of this repository.
    * Click "Install to Workspace"
1. Create authentication tokens
    * App-level token: Scroll down to "App-Level Tokens" and generate a new token
      with the `connections:write` scope. The new token should start with `xapp-...`.
    * Bot token: Go to "OAuth & Permissions", a bot token should have
      already been generated for you, it starts with `xoxb-...`.
    * Export those tokens as environment variables, e.g. by adding
      them to your `~/.bashrc`, `~/.zshrc` or similar.
      ```
      export SLACK_TZ_APP_TOKEN='xapp-...'
      export SLACK_TZ_BOT_TOKEN='xoxb-...'
      ```
1. Add the bot to a channel
    * Open up the Slack client and log into the workspace you created in step 1.
    * Right click on a channel name and select
      "Open channel details" > "Integrations" > "Add apps" > "tzbot"
1. Run the bot with `make run` (or `cabal run tzbot-exe` / `stack run`).

 [time-zones-offsets]: https://spin.atomicobject.com/2016/07/06/time-zones-offsets
 [tzlabel]: https://hackage.haskell.org/package/tz-0.1.3.6/docs/Data-Time-Zones-All.html#t:TZLabel
