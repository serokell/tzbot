-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module TzBot.Util where

import Universum

attach :: (Functor f) => (a -> b) -> f a -> f (a, b)
attach f = fmap (\x -> (x, f x))
