-- grid is a game written in Haskell
-- Copyright (C) 2018 karamellpelle@hotmail.com
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
module Game.Run.Scene
  (

#ifdef GRID_STYLE_FANCY
    module Game.Run.Scene.Fancy,
#endif
#ifdef GRID_STYLE_PLAIN
    module Game.Run.Scene.Plain,
#endif

  ) where

#ifdef GRID_STYLE_FANCY
import Game.Run.Scene.Fancy
#endif
#ifdef GRID_STYLE_PLAIN
import Game.Run.Scene.Plain
#endif
