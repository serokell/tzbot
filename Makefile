# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

.PHONY: tzbot test haddock haddock-no-deps stylish lint clean bats all

# Build target from the common utility Makefile
MAKEU = $(MAKE) -C make/

MAKE_PACKAGE = $(MAKEU) PACKAGE=tzbot

tzbot:
	$(MAKE_PACKAGE) dev
test:
	$(MAKE_PACKAGE) test
test-dumb-term:
	$(MAKE_PACKAGE) test-dumb-term
test-hide-successes:
	$(MAKE_PACKAGE) test-hide-successes
haddock:
	$(MAKE_PACKAGE) haddock
haddock-no-deps:
	$(MAKE_PACKAGE) haddock-no-deps
clean:
	$(MAKE_PACKAGE) clean
run:
	$(MAKEU) PACKAGE=tzbot-exe run

stylish:
	find . -name '.stack-work' -prune -o -name '.dist-newstyle' -prune -o -name '*.hs' -exec stylish-haskell -i '{}' \;

lint:
	hlint .

all:
	$(MAKEU) PACKAGE=""
test-all:
	$(MAKEU) test PACKAGE=""
