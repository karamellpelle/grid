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
module Game.Memory.Do.Grid
  (
    doGridShow,
    doGridPlay,
    doGridFailure,

  ) where

import MyPrelude
import Game

import Game.Grid
import Game.Grid.Modify
import Game.Grid.Do
import Game.Grid.StepDT
import Game.Memory



doGridShow :: s -> GridWorld -> MemoryWorld -> MEnv' (s, GridWorld, MemoryWorld)
doGridShow =
    defaultDo (controlGrid controlCamera)
              (tickClockMemoryGet, tickClockMemorySet)
              (defaultStepDT collisionShow)
              (noBreakModify)
              (noDefaultModify)


doGridPlay :: s -> GridWorld -> MemoryWorld -> MEnv' (s, GridWorld, MemoryWorld)
doGridPlay =
    defaultDo (controlGrid controlCameraPathContinue)
              (tickClockMemoryGet, tickClockMemorySet)
              (defaultStepDT collisionPlay)
              (noBreakModify)
              (noDefaultModify)


doGridFailure :: s -> GridWorld -> MemoryWorld -> MEnv' (s, GridWorld, MemoryWorld)
doGridFailure = 
    doGridShow




--------------------------------------------------------------------------------
--  

collisionShow :: Collision s MemoryWorld
collisionShow =
    Collision
    { 
        onPathNode = \path s grid mem -> 
            case memoryFood mem of
                
                -- all food eaten. halt.
                []      -> do
                    let path' = path { pathAlpha = 1.0 }
                    return (path', s, grid, worldPushEvent mem EventAllFoodEaten)

                -- continue with next food as current segment
                (f:fs)  -> do
                    path' <- pathEatTurn path f
                    return (path', s, grid, mem { memoryFood = fs })

                                
    }
                                
                            

collisionPlay :: Collision s MemoryWorld
collisionPlay = 
    Collision
    { 
        onPathNode = \path s grid mem -> if memoryIsFailure mem 

            -- stop when failure
            then return (path, s, grid, mem)

            else case memoryFood mem of

                -- all food eaten 
                []      -> do
                    let path' = path { pathAlpha = 1.0 }
                    return (path', s, grid, worldPushEvent mem EventAllFoodEaten)

                -- path eats a new Segment. but is it correct?
                (f:fs)  -> do
                    path' <- pathEatContinue path
                    let t0 = pathTurn path
                        t1 = pathTurn path'
                    if direction (turnDiff t0 t1) == direction f

                      -- correct turn
                      then let (path'', mem') = handleCorrect f fs path' mem
                           in  return (path'', s, grid, mem') 
                      
                      
                      -- wrong turn
                      else let (path'', mem') = handleWrong f fs path' mem
                           in  return (path'', s, grid, mem')

    }
    
    where
      handleCorrect f fs path mem =
          (path, mem { memoryFood = fs })

      handleWrong f fs path mem =
          let path' = path { pathCurrent = segmentEatTurn (path0Last path) f }
              mem' = mem
                     {
                         memoryFood = fs,
                         memoryIsFailure = True,
                         memoryFailureIx = pathSize path',
                         memoryFailureSegment = pathCurrent path,
                         memoryEvents = memoryEvents mem ++ [EventWrongTurn]
                     }
          in  (path', mem')
          
