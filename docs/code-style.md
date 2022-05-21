<!--
-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0
-->

Our style guides can be found in [the respective repository](https://github.com/serokell/style).

We make a step away from those guidelines as described below.

## Haskell

### Export lists

It is allowed to have no explicit export list in the following situations:
* The module is internal;
* The module assumes exporting all its declarations;
* The module resides in tests.
