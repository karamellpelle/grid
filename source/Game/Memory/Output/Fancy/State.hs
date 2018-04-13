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
module Game.Memory.Output.Fancy.State
  (
    stateSetBeginShow,

  ) where

import MyPrelude
import Game

import Game.Memory
import Game.Memory.MemoryWorld.OutputState
import Game.Run.RunWorld

import System.Random



stateSetBeginShow :: MemoryWorld -> RunWorld -> IO (MemoryWorld, RunWorld)
stateSetBeginShow mem run = do
    mem' <- modifyIO mem $ \state -> do
            plus <- randomRIO (0, valueColorMapSize - 2)
            let ix1 = (ostateColorIx1 state + plus) `mod` valueColorMapSize
            return state
                   {
                      ostateColorIx0 = ostateColorIx1 state,
                      ostateColorIx1 = ix1
                   }

    return (mem', run)     



modifyIO :: MemoryWorld -> (OutputState -> IO OutputState) -> IO MemoryWorld
modifyIO mem f = do
    state' <- f $ memoryOutputState mem
    return mem { memoryOutputState = state' }


