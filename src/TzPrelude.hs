-- SPDX-FileCopyrightText: 2023 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzPrelude
  ( module U

  ) where

-- Hide deprecated functions/types.
import Universum as U hiding
  (Lens, Lens', Traversal, Traversal', _1, _2, _3, _4, _5, over, preuse, preview, set, use, view,
  (%~), (.~), (^.), (^..), (^?))
