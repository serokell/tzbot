-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Options where

import TzPrelude hiding (asum)

import Options.Applicative

data Command
  = DefaultCommand Options
  | DumpConfig DumpOptions

data DumpOptions
  = DOStdOut
  | DOFile FilePath Bool

data Options = Options
  { oConfigFile :: Maybe FilePath
  }

totalParser :: ParserInfo Command
totalParser = info (commandParserWithDefault <**> helper) $
  mconcat
  [ fullDesc
  , progDesc
      "Perform time references translation on new messages post to \
       \Slack conversations or on direct user triggers."
  , header "Slack timezone bot"
  , footer configAndEnvironmentNote
  ]

commandParserWithDefault :: Parser Command
commandParserWithDefault = asum
  [ commandParser
  , DefaultCommand <$> optionsParser
  ]

commandParser :: Parser Command
commandParser = hsubparser $
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

optionsParser :: Parser Options
optionsParser = Options <$> do
  optional $
    strOption
      (long "config" <> short 'c' <> metavar "FILEPATH" <> help "Load configuration from FILEPATH")

configAndEnvironmentNote :: String
configAndEnvironmentNote =
  "Configuration parameters can be also specified using environment\
  \ variables, for details run `tzbot dump-config -f <filepath>` and\
  \ see the config fields descriptions. If all the parameters are contained\
  \ by either envvars or the default config, the additional config file is\
  \ not required."
