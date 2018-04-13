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
module Game.LevelPuzzleMode.Iteration
  (
    iterationBeginPlay,
    iterationPlay,
    iterationComplete,
    iterationFailure,

  ) where


import MyPrelude
import Game


import Game.LevelPuzzleMode.LevelPuzzleWorld
import Game.LevelPuzzleMode.Helpers
import Game.LevelPuzzleMode.Output
import Game.LevelPuzzleMode.Do
import Game.LevelPuzzleMode.Iteration.State

import Game.Grid.Helpers
import Game.Run.RunWorld
import Graphics.UI.GLFW

--------------------------------------------------------------------------------
--  iterationPlay


-- | setup and play current level 
iterationBeginPlay :: Iteration LevelPuzzleWorld RunWorld
iterationBeginPlay =
    makeIteration $ \lvl run -> do
        (lvl', run') <- outputBeginPlay lvl run

        case levelpuzzleIsComplete lvl' of
            False -> do
                lvl'' <- levelpuzzleBeginLevel lvl'

                -- set tick to world tick of LevelPuzzleWorld
                tickClockLevelPuzzleModeSet $ worldTick lvl''
        
                iteration iterationPlay lvl'' run'
              
            True -> 
                iteration iterationComplete lvl' run'



iterationPlay :: Iteration LevelPuzzleWorld RunWorld
iterationPlay =
    makeIteration $ \lvl run -> do

        (lvl', run') <- outputPlay lvl run

        iteration (iterationPlay' ()) lvl' run'


iterationPlay' :: s -> Iteration LevelPuzzleWorld RunWorld
iterationPlay' s =
    defaultIteration s outputPlay' $ defaultStep doPlay $ \s lvl run -> do
        handleEventsBreakM lvl (lvl, run, [iterationPlay' s]) $ 
                           \lrt@(lvl, run, top) event -> case event of

            -- DotFinish => next level
            EventPathEatDotFinish -> do
                ---- save PlayerPath
                --playersLocalPlayer >>= \maybe -> case maybe of
                --    case maybe of
                --        Nothing     -> savePlayerPathEmpty lvl
                --        Just player -> savePlayerPath player lvl

                lvl' <- levelpuzzleSetLevelNext lvl

                      
                breakEventsM (lvl', run, [iterationBeginPlay])

            -- Wall => iterationFailure (unless success at same time)
            EventPathEatWall       -> 
                continueEventsM (lvl, run, [iterationFailure event, iterationBeginPlay])
           
            -- no more segments  => iterationFailure (unless success at same time)
            EventNullSegments -> 
                continueEventsM (lvl, run, [iterationFailure event, iterationBeginPlay])


            _                   ->
                continueEventsM lrt

        


--------------------------------------------------------------------------------
--  iterationComplete


iterationComplete :: Iteration LevelPuzzleWorld RunWorld
iterationComplete = 
    makeIteration $ \lvl run -> do
        
        (lvl', run') <- outputComplete lvl run
        
        iteration (iterationComplete' ()) lvl' run'
    where
      t0 = 16.0
      s0 = 16.0
      turn0 = upTurn
      view0 = View 0.0 0.0 128.0



iterationComplete' :: s -> Iteration LevelPuzzleWorld RunWorld
iterationComplete' s =
    defaultIteration s outputComplete' $ defaultStep doComplete $ \s lvl run -> do
        
        -- game ends here!        
        return (lvl, run, [iterationComplete' s]) 




--------------------------------------------------------------------------------
--  iterationFailure



iterationFailure :: LevelPuzzleEvent -> Iteration LevelPuzzleWorld RunWorld
iterationFailure event = 
    makeIteration $ \lvl run -> do
        
        (lvl', run') <- outputFailure lvl run
     
        s <- makeFailureSPuzzleMode event 0.0 
        iteration (iterationFailure' s) (lvl' { levelpuzzleFailureEvent = event }) run'



iterationFailure' :: FailureS -> Iteration LevelPuzzleWorld RunWorld
iterationFailure' s =
    defaultIteration s outputFailure' $ defaultStep doFailure $ \s lvl run -> do
          
          keysKeyOnce (SpecialKey ENTER) >>= \bool -> case bool of
              False   -> return (lvl, run, [iterationFailure' s])
              True    -> return (lvl, run, [])


