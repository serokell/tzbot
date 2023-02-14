-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Options where

import Universum

import Options.Applicative
import Text.Interpolation.Nyan (int)

data Command
  = RunServer RunServerOptions
  | RunSocketMode RunSocketModeOptions
  | DumpConfig DumpOptions

data DumpOptions
  = DOStdOut
  | DOFile FilePath Bool

data RunServerOptions = RunServerOptions
  { rsoConfigFile   :: Maybe FilePath
  , rsoVerification :: Bool
  }

newtype RunSocketModeOptions = RunSocketModeOptions
  { rsmoConfigFile   :: Maybe FilePath
  }

totalParser :: ParserInfo Command
totalParser = info (commandParserWithDefault <**> helper) $
  mconcat
  [ fullDesc
  , progDesc
    [int|n|
      Perform time references translation on new messages post to
      Slack conversations or on direct user triggers.
      |]
  , header "Slack timezone bot"
  , footer configAndEnvironmentNote
  ]

----------------------------------------------------------------------------
---- Commands
----------------------------------------------------------------------------
commandParserWithDefault :: Parser Command
commandParserWithDefault = asum
  [ dumpCommandParser
  , runServerCommandParser
  , runSocketModeParser
  ]

dumpCommandParser :: Parser Command
dumpCommandParser = hsubparser $
  command "dump-config" $
    info (DumpConfig <$> dumpOptionsParser) (progDesc "Dump default config")

dumpOptionsParser :: Parser DumpOptions
dumpOptionsParser = asum [stdoutParser, dumpFileParser]
  where
    stdoutParser = flag' DOStdOut $ long "stdout" <> help "Standart output"
    dumpFileParser = DOFile <$> strOption fileOption <*> forceOption
    fileOption :: Mod OptionFields FilePath
    fileOption = (long "file" <> short 'f' <> metavar "FILEPATH" <> help "Dump to file FILEPATH")
    forceOption = switch (long "force" <> help "Whether to overwrite existing file")

runServerCommandParser :: Parser Command
runServerCommandParser = hsubparser $
  command "server" $
    info (RunServer <$> runServerOptionsParser) (progDesc "Run the bot as a server")

runServerOptionsParser :: Parser RunServerOptions
runServerOptionsParser = do
  rsoConfigFile <- optional configOptionParser
  rsoVerification <- switch (long "verification" <> help "Run server in the verification mode")
  pure RunServerOptions {..}

runSocketModeParser :: Parser Command
runSocketModeParser = hsubparser $
  command "socket-mode" $
    info (RunSocketMode <$> runSocketModeOptionsParser) (progDesc "Run the bot in the socket mode")

runSocketModeOptionsParser :: Parser RunSocketModeOptions
runSocketModeOptionsParser = RunSocketModeOptions <$> optional configOptionParser

----------------------------------------------------------------------------
---- Common
----------------------------------------------------------------------------
configOptionParser :: Parser FilePath
configOptionParser = strOption
  (long "config" <> short 'c' <> metavar "FILEPATH"
    <> help "Load configuration from FILEPATH")

----------------------------------------------------------------------------
---- Footer
----------------------------------------------------------------------------
configAndEnvironmentNote :: String
configAndEnvironmentNote =
  [int|n|
    Configuration parameters can be also specified using environment
    variables, for details run `tzbot dump-config -f <filepath>` and
    see the config fields descriptions. If all the parameters are contained
    by either envvars or the default config, the additional config file is
    not required.
    |]
