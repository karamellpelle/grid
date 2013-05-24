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
module LevelTools.Do.Grid
  (
    doGridEdit,

  ) where

import MyPrelude
import Game

import Game.Grid.Modify
import Game.Grid.Do
import Game.Grid.StepDT
import Game.Grid.GridWorld
import Game.Grid.Helpers

import LevelTools.EditWorld


--doGridPlay :: b -> GridWorld -> a -> MEnv' (b, GridWorld, a)
doGridEdit =
    defaultDo (controlGrid controlCamera)
              
              (tickClockFGet, tickClockFSet, valueDTUnit, valueMaxElaps)
              (cameraStepDT)
              (noBreakModify)
              (postStepDTModify)


