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
module Game.LevelPuzzleMode.Do.Grid.Modify
  (
    controlPathPlay,
    --controlPathSpecialComplete,

  ) where

import MyPrelude
import Game
import Game.Grid.GridWorld
import Game.Grid.Modify
import Game.LevelPuzzleMode.LevelPuzzleWorld


controlPathPlay :: s -> GridWorld -> LevelPuzzleWorld -> 
                      MEnv' (s, GridWorld, LevelPuzzleWorld)
controlPathPlay = \s grid lvl -> do
    -- camera input
    grid' <- inputCamera grid
    
    -- path input
    grid'' <- case levelpuzzleIsPuzzle lvl of
              False -> inputPathContinue grid'
              True  -> inputPathWait grid'

    return (s, grid'', lvl)
