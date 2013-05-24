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
module Game.LevelPuzzleMode.Do.Grid
  (
    doGridPlay,
    doGridFailure,
    doGridComplete,
    --doGridSpecialComplete,

  ) where

import MyPrelude
import Game
import Game.Grid.Helpers 
import Game.Grid.Modify
import Game.Grid.Do
import Game.Grid.StepDT
import Game.LevelPuzzleMode.LevelPuzzleWorld
import Game.LevelPuzzleMode.Helpers
import Game.LevelPuzzleMode.Do.Grid.Modify
import Game.LevelPuzzleMode.Do.Grid.StepDT




-- | Play
doGridPlay :: s -> GridWorld -> LevelPuzzleWorld -> 
              MEnv' (s, GridWorld, LevelPuzzleWorld)
doGridPlay =
    defaultDo (controlGrid controlPathPlay)
              (tickClockLevelPuzzleModeGet, tickClockLevelPuzzleModeSet, valueDTUnit, valueMaxElaps)
              (defaultStepDT collisionPlay)
              (noBreakModify)
              (postStepDTModify)



-- | Failure
doGridFailure ::  s -> GridWorld -> LevelPuzzleWorld -> 
                  MEnv' (s, GridWorld, LevelPuzzleWorld)
doGridFailure =
    defaultDo (controlGrid controlCamera) 
              (tickClockLevelPuzzleModeGet, tickClockLevelPuzzleModeSet, valueDTUnit, valueMaxElaps)
              (cameraStepDT)
              (noBreakModify)
              (postStepDTModify)


-- | Complete
doGridComplete ::  s -> GridWorld -> LevelPuzzleWorld -> 
                   MEnv' (s, GridWorld, LevelPuzzleWorld)
doGridComplete =
    defaultDo (controlGrid controlCamera)
              (tickClockLevelPuzzleModeGet, tickClockLevelPuzzleModeSet, valueDTUnit, valueMaxElaps)
              (cameraStepDT)
              (noBreakModify)
              (postStepDTModify)
{-              
-- | SpecialComplete
doGridSpecialComplete ::  s -> GridWorld -> LevelPuzzleWorld -> 
                          MEnv' (s, GridWorld, LevelPuzzleWorld)
doGridSpecialComplete =
    defaultDo (controlGrid controlRunCamera)
              (tickClockLevelPuzzleModeGet, tickClockLevelPuzzleModeSet, valueDTUnit, valueMaxElaps)
              (cameraStepDT)
              (noBreakModify)
              (postStepDTModify)
-}            
