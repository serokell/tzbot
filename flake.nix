# SPDX-FileCopyrightText: 2023 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0
{
  nixConfig = {
    flake-registry = "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";
  };

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      flake = false;
    };
    haskell-nix = {
      inputs.hackage.follows = "hackage";
      inputs.stackage.follows = "stackage";
    };
    hackage = {
      flake = false;
    };
    stackage = {
      flake = false;
    };
  };

  outputs = { self, nixpkgs, haskell-nix, hackage, stackage, serokell-nix, flake-compat, flake-utils, ... }@inputs:
  flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      haskellPkgs = haskell-nix.legacyPackages."${system}";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          serokell-nix.overlay
        ];
      };

      lib = pkgs.lib;

      hs-package-name = "tzbot";

      # invoke haskell.nix
      hs-pkgs = haskellPkgs.haskell-nix.stackProject {
        src = haskellPkgs.haskell-nix.haskellLib.cleanGit {
          name = hs-package-name;
          src = ./.;
        };

        # haskell.nix configuration
        modules = [{
        packages.tzbot = {
          ghcOptions = [
            "-Werror"
          ];

          # enable haddock for local packages
          doHaddock = true;
        };

        # disable haddock for dependencies
        doHaddock = false;
        }];
      };

      hs-pkg = hs-pkgs.${hs-package-name};

      # returns the list of all components for a package
      get-package-components = pkg:
        # library
        lib.optional (pkg ? library) pkg.library
        # haddock
        ++ lib.optional (pkg ? library) pkg.library.haddock
        # exes, tests and benchmarks
        ++ lib.attrValues pkg.exes
        ++ lib.attrValues pkg.tests
        ++ lib.attrValues pkg.benchmarks;

      # all components for the current haskell package
      all-components = get-package-components hs-pkg.components;

      stack2cabal = haskellPkgs.haskell.lib.overrideCabal haskellPkgs.haskellPackages.stack2cabal
        (drv: { jailbreak = true; broken = false; });

    in {
      # nixpkgs revision pinned by this flake
      legacyPackages = pkgs;

      # derivations that we can run from CI
      checks = {
        # builds all haskell components
        build-all = pkgs.linkFarmFromDrvs "build-all" all-components;

        # runs the test
        test = hs-pkg.checks.tzbot-test;

        trailing-whitespace = pkgs.build.checkTrailingWhitespace ./.;
        reuse-lint = pkgs.build.reuseLint ./.;
        shellcheck = pkgs.build.shellcheck ./.;

        hlint = pkgs.build.haskell.hlint ./.;
        stylish-haskell = pkgs.build.haskell.stylish-haskell ./.;
      };
      devShells = {
        ci = pkgs.mkShell {
          buildInputs = [
            stack2cabal
          ];
        };
        doctest = hs-pkgs.shellFor {
          withHoogle = false;
          # https://github.com/input-output-hk/haskell.nix/issues/839
          exactDeps = true;
          additional = _ : [ hs-pkg.components.library ];
        };
      };
      packages = {
        tzbot = hs-pkg.components.exes.tzbot-exe;
      };
    });
}
