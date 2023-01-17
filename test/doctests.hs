-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Main where

import Universum

import System.Environment (getArgs)
import Test.DocTest

main :: IO ()
main = getArgs >>= mainFromCabal "tzbot"
