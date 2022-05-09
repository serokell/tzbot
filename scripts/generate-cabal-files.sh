#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

# This script generates cabal.project, cabal.project.freeze and *.cabal
# files which can be used in order to build the project using cabal

set -e

[ -f cabal.project ] && rm cabal.project
[ -f cabal.project.freeze ] && rm cabal.project.freeze

mkdir -p stack2cabal
stack install stack2cabal --resolver snapshot-stack2cabal.yaml --local-bin-path stack2cabal
./stack2cabal/stack2cabal
rm -rf stack2cabal
echo "cabal.project, cabal.project.freeze and *.cabal files are successfully generated"
