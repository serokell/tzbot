-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.BotMain where

import Universum

import Data.ByteString qualified as BS
import Options.Applicative (execParser)
import System.Directory (doesFileExist)
import Text.Interpolation.Nyan (int, rmode')

import TzBot.BotMain.Server (runServer)
import TzBot.BotMain.Server.Verification (runVerificationServer)
import TzBot.BotMain.SocketMode (runSocketMode)
import TzBot.Config.Default (defaultConfigText)
import TzBot.Options

{- |
Usage:

See @Config.Default.defaultConfigText@ to get what options
are available and how to set them via config or via envvars.
To generate app-level / bot tokens, see: <docs/development.md>

-}
main :: IO ()
main = do
  cliOptions <- execParser totalParser
  case cliOptions of
    DumpConfig dumpOpts -> dumpConfig dumpOpts
    RunSocketMode opts -> runSocketMode opts
    RunServer opts ->
      if rsoVerification opts
      then runVerificationServer opts
      else runServer opts

dumpConfig :: DumpOptions -> IO ()
dumpConfig = \case
  DOStdOut -> putStr defaultConfigText
  DOFile path force -> do
    let writeAction = BS.writeFile path defaultConfigText
    if force
    then writeAction
    else ifM
      (doesFileExist path)
      (hPutStrLn @Text stderr [int||File #{path} already exists, \
                                use --force to overwrite|] >> exitFailure)
      writeAction
