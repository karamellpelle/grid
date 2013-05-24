-- grid is a game written in Haskell
-- Copyright (C) 2013 Carl Joachim Svenn
-- 
-- This file is part of grid.
-- 
-- grid is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- grid is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with grid.  If not, see <http://www.gnu.org/licenses/>.
--
module Game.Data.View
  (
    View (..),
    viewInv,

  ) where

import MyPrelude

data View =
    View
    {
        viewA :: !Float,
        viewB :: !Float,
        viewC :: !Float
    }


-- View is a monoid
instance Monoid View where
    mempty = 
        View 0.0 0.0 0.0
    mappend (View a1 b1 c1) (View a0 b0 c0) = 
        View (a1 + a0) (b1 + b0) (c1 + c0)

-- View also a group
viewInv :: View -> View
viewInv (View a b c) =
    View (negate a) (negate b) (negate c)

