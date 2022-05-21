# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{ linux ? false, linux-static ? false, windows ? false }:
let
  sources = import ./nix/sources.nix;
  weeder-hacks = import sources.haskell-nix-weeder { inherit pkgs; };
  nixpkgs = (import ./ci.nix).pkgs;
  src = (import ./ci.nix).project-src;
  pkgs = if linux-static then nixpkgs.pkgsCross.musl64 else if windows then nixpkgs.pkgsCross.mingwW64 else nixpkgs;
  project = pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    modules = [{
      packages.tzbot = {
        ghcOptions = [
          "-Werror"
          # produce *.dump-hi files, required for weeder
          "-ddump-to-file" "-ddump-hi"
        ];

        # collect all *.dump-hi files (required for weeder)
        postInstall = weeder-hacks.collect-dump-hi-files;

        # enable haddock for local packages
        doHaddock = true;
      };

      # disable haddock for dependencies
      doHaddock = false;
    }];
  };
in project
