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
module Game.Memory.Iteration
  (
    iterationBeginShow,
    iterationShow,
    iterationBeginPlay,
    iterationPlay,
    iterationFailure,

  ) where

import MyPrelude
import Game

import Game.Grid
import Game.Memory
import Game.Memory.Output
import Game.Memory.Do
import Game.Run.RunWorld


-- fixme: iterationBeginPlay into iterationPlay

--------------------------------------------------------------------------------
--  iterationBeginShow


-- | setup and show current level
iterationBeginShow :: Iteration MemoryWorld RunWorld
iterationBeginShow =
    makeIteration $ \mem run -> do

        (mem', run') <- outputBeginShow mem run

        iteration iterationShow mem' run'


--------------------------------------------------------------------------------
--  iterationShow


iterationShow :: Iteration MemoryWorld RunWorld
iterationShow = 
    makeIteration $ \mem run -> do

        (mem', run') <- outputShow mem run

        -- clear keys
        keysClear

        -- begin level
        mem'' <- memorySetLevelBegin mem'
        
        -- set tick to world tick of MemoryWorld
        tickClockMemorySet $ worldTick mem''

        iteration (iterationShow' False) mem'' run'



iterationShow' :: Bool -> Iteration MemoryWorld RunWorld
iterationShow' isComplete =
    defaultIteration isComplete outputShow' $ defaultStep doShow 
                                            $ \isComplete mem run -> do
        
        let isComplete' = handleAllEvents mem isComplete $ \isComplete event -> 
                          case event of
                              EventAllFoodEaten   -> True
                              _                   -> isComplete

        -- interactive when Show finished
        if isComplete' 
          then do
              -- ButtonA => iterationBeginPlay
              keysTouchHandleButtonA (mem, run, [iterationShow' isComplete']) $ \_ -> 
                                     (mem, run, [iterationBeginPlay])

          else return (mem, run, [iterationShow' isComplete'])



--------------------------------------------------------------------------------
--  iterationBeginPlay

iterationBeginPlay :: Iteration MemoryWorld RunWorld
iterationBeginPlay = 
    makeIteration $ \mem run -> do

        (mem', run') <- outputBeginPlay mem run

        -- clear keys
        keysClear

        -- begin level
        mem'' <- memorySetLevelBegin mem'

        -- set tick to world tick of MemoryWorld
        tickClockMemorySet $ worldTick mem''

        iteration (iterationPlay) mem'' run


--------------------------------------------------------------------------------
--  iterationPlay

iterationPlay :: Iteration MemoryWorld RunWorld
iterationPlay = 
    makeIteration $ \mem run -> do

        (mem', run') <- outputPlay mem run

        iteration (iterationPlay' ()) mem' run'


iterationPlay' :: s -> Iteration MemoryWorld RunWorld
iterationPlay' s = 
    defaultIteration s outputPlay' $ defaultStep doPlay $ \s mem run -> do
       
        -- now look at events
        handleAllEventsM mem (mem, run, [iterationPlay' s]) $ 
            \mrt@(mem, run, top) event -> case event of
             
                -- all segments eaten => next level
                EventAllFoodEaten  -> do
                    mem' <- memorySetLevelNext mem
                    return (mem', run, [iterationBeginShow])

                -- wrong segment eaten => iterationFailure
                EventWrongTurn     -> do
                    return (mem, run, [iterationFailure])

                _                  -> 
                    return mrt


       





--------------------------------------------------------------------------------
--  iterationFailure


iterationFailure :: Iteration MemoryWorld RunWorld
iterationFailure = 
    makeIteration $ \mem run -> do
        
        (mem', run') <- outputFailure mem run

        -- clear keys
        keysClear
        
        let mem'' = mem' { memoryFailureTick = worldTick mem' }

        iteration (iterationFailure' ()) mem'' run'



iterationFailure' :: s -> Iteration MemoryWorld RunWorld
iterationFailure' s =
    defaultIteration s outputFailure' $ defaultStep doFailure 
                                      $ \tick mem run -> do 

        -- ButtonA => iterationBeginPlay
        (mem', run', top') <-
            keysTouchHandleButtonA (mem, run, [iterationFailure' s]) $ \pos ->
                                   (mem, run, [iterationBeginPlay])

        -- ButtonB => iterationBeginShow
        (mem'', run'', top'') <-
            keysTouchHandleButtonB (mem', run', top') $ \pos ->
                                   (mem', run', [iterationShow])

        return (mem'', run'', top'')



