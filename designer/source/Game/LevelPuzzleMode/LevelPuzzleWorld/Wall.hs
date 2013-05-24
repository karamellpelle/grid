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
module Game.LevelPuzzleMode.LevelPuzzleWorld.Wall
  (
    Wall (..),
    makeWall,

  ) where

import MyPrelude
import Game.Grid.GridWorld



data Wall =
    Wall
    {
        wallIsDouble :: !Bool,
        wallNode  :: !Node,
        wallX :: !Node,
        wallY :: !Node
    }


makeWall :: Bool -> Node -> Node -> Node -> Wall
makeWall dbl node x y =
    Wall dbl node x y 



