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
module MEnv.Env
  (
    Env (..),
    EnvInit (..),

    module MEnv.Env.ScreenObject,
    module MEnv.Env.SoundObject,
    module MEnv.Env.TickObject,
    module MEnv.Env.KeysObject,
    module MEnv.Env.ForeignObject,
    module MEnv.Env.PlayersObject,
    module MEnv.Env.SystemObject,

  ) where


import MEnv.Env.ScreenObject
import MEnv.Env.SoundObject
import MEnv.Env.TickObject
import MEnv.Env.KeysObject
import MEnv.Env.ForeignObject
import MEnv.Env.PlayersObject
import MEnv.Env.SystemObject



data EnvInit = 
    EnvInit
    {
        initScreen :: !ScreenInit,
        initSound :: !SoundInit,
        initTick :: !TickInit,
        initKeys :: !KeysInit,
        initForeign :: !ForeignInit,
        initPlayers :: !PlayersInit,
        initSystem:: !SystemInit
    }



data Env res = 
    Env
    {
        envScreen :: !ScreenObject,
        envSound :: !SoundObject,
        envTick :: !TickObject,
        envKeys :: !KeysObject,
        envForeign :: !ForeignObject,
        envPlayers :: !PlayersObject,
        envSystem :: !SystemObject,
        envResource :: !res
    }

