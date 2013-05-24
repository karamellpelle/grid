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
module Game.Memory.Output.Plain.State
  (
    stateSetBeginShow,
    stateStep,

  ) where

import MyPrelude
import Game

import Game.Memory
import Game.Memory.MemoryWorld.OutputState
import Game.Run.RunWorld

import System.Random



stateSetBeginShow :: MemoryWorld -> RunWorld -> IO (MemoryWorld, RunWorld)
stateSetBeginShow mem run = do
    mem' <- modifyIO mem $ \state -> if outputstateLevelIx state == memoryLevelIx mem
            then return state
            else do
                plus <- randomRIO (0, valueColorMapSize - 2)
                let ix' = (outputstateColorIx' state + plus) `mod` valueColorMapSize
                return state
                       {
                          outputstateLevelIx = memoryLevelIx mem,
                          outputstateColorIx = outputstateColorIx' state,
                          outputstateColorIx' = ix',
                          outputstateTick = worldTick mem,
                          outputstateAlpha = 0.0
                       }

    return (mem', run)     


stateStep :: s -> MemoryWorld -> RunWorld -> IO (s, MemoryWorld, RunWorld)
stateStep s mem run = do
    mem' <- modifyIO mem $ step (worldTick mem)
    return (s, mem', run) 

--------------------------------------------------------------------------------
--  

step :: Tick -> OutputState -> IO OutputState
step tick = \state -> 
    let dt = rTF (tick - outputstateTick state)
    in  if unit <= dt
        then let alpha' = min 1.0 $ outputstateAlpha state + scale * dt
             in  return state { outputstateAlpha = alpha' }
        else return state

    where
      scale = 0.0625
      unit = 0.08


modifyIO :: MemoryWorld -> (OutputState -> IO OutputState) -> IO MemoryWorld
modifyIO mem f = do
    state' <- f $ memoryOutputState mem
    return mem { memoryOutputState = state' }


