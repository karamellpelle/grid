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
module Game.MEnv
  (
    MEnv',

    module MEnv,
    module MEnv.Tick,
    module MEnv.Keys,
    module MEnv.Screen,
    module MEnv.Sound,
    module MEnv.Foreign,
    module MEnv.Players,
    module MEnv.System,
    module MEnv.Resource,
    module Game.GameData,

  ) where


import MEnv
import MEnv.Tick
import MEnv.Keys
import MEnv.Screen
import MEnv.Sound
import MEnv.Foreign
import MEnv.Players
import MEnv.System
import MEnv.Resource
import Game.GameData



type MEnv' =
    MEnv GameData




