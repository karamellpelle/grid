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
module Game.Run.Iteration.Foreign
  (
    iterationForeign,

  ) where


import MyPrelude
import Game

import Game.Run.RunWorld
import Game.Run.Do
import Game.Run.Output
import Game.Run.Helpers
import Game.Grid.Helpers

import OpenGL
import OpenGL.Helpers




--------------------------------------------------------------------------------
--  iterationForeign

iterationForeign :: Iteration' RunWorld
iterationForeign = 
    makeIteration' $ \run -> do
        outputForeign run

        -- begin Foreign platform mode
        foreignBeginForeign

        iteration' (iterationForeign' ()) run


iterationForeign' :: s -> Iteration' RunWorld
iterationForeign' s = 
    defaultIteration s noOutput $ \s run b -> do

        -- if Foreign ended, take screenshot and finish
        join $ foreignHandleForeignEnd (return (run, b, [iterationForeign' s])) $ do

            -- grab screenshot
            run' <- screenshotForeign run

            return (run', b, [])




