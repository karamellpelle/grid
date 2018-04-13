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
module Game.LevelPuzzle.Iteration
  (
    iterationBeginPlay,
    iterationPlay,
    iterationComplete,
    iterationFailure,

  ) where


import MyPrelude
import Game

import Game.Grid
import Game.LevelPuzzle
import Game.LevelPuzzle.Output
import Game.LevelPuzzle.Do
import Game.LevelPuzzle.Iteration.State

import Game.Run.RunWorld
import Game.Run.Helpers




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
                tickClockLevelPuzzleSet $ worldTick lvl''
        
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
            EventPathEatDotFinish dot -> do

                -- it would be nice to save the solution of this level, letting
                -- players co-operate via GameCenter (ask a friend for help)
                ---- save PlayerPath (write Path to file)
                --playersLocalPlayer >>= \maybe -> case maybe of
                --    case maybe of
                --        Nothing     -> savePlayerPathEmpty lvl
                --        Just player -> savePlayerPath player lvl

                lvl' <- levelpuzzleSetLevelNext lvl

                -- if next level is not puzzle, switch to LevelMode
                if not (levelpuzzleLevelPuzzleTag lvl')
                  then breakEventsM (lvl', worldPushEvent run EventLevelMode, [iterationBeginPlay])
                      
                  else breakEventsM (lvl', run, [iterationBeginPlay])
                
                
            -- Wall => iterationFailure (unless success at same time)
            EventPathEatWall wall  -> 
                let lvl' = handleEatWall lvl event
                in  continueEventsM (lvl', run, [iterationFailure, iterationBeginPlay])


            -- no more segments  => iterationFailure (unless success at same time)
            EventNullSegments -> 
                let lvl' = handleNullSegments lvl event
                in  continueEventsM (lvl', run, [iterationFailure, iterationBeginPlay])


            _                           ->
                continueEventsM lrt

        
    where
      handleEatWall lvl event = 
          levelpuzzleModifyPath (lvl { levelpuzzleFailureEvent = event }) $ 
              \path -> path { pathWaiting = True }

      handleNullSegments lvl event = 
          lvl { levelpuzzleFailureEvent = event }



--------------------------------------------------------------------------------
--  iterationComplete

iterationComplete :: Iteration LevelPuzzleWorld RunWorld
iterationComplete = 
    makeIteration $ \lvl run -> do
        
        (lvl', run') <- outputComplete lvl run

        run'' <- if levelpuzzleIsSpecial lvl'
                 then do
                    -- send achievement 
                    unless (runSpecialIsCompleted run') $ sendAchievementSpecialComplete
                        
                    return run' { runSpecialIsCompleted = True }
                 
                 else return run'

        let lvl'' = setCamera lvl'
            s = makeCompleteS lvl'' run''

        iteration (iterationComplete' s) lvl'' run''

    where
      sendAchievementSpecialComplete = 
          playersSendAchievement valuePlayersAchievementSpecialLevelPuzzleWorldCompleted 1.0

      setCamera lvl =
          let turn = turnInverse $ levelpuzzleRefSpace lvl
              --lvl' = levelpuzzleModifyCamera lvl $ \cam -> cameraSetTurn cam turn
              View a b c = cameraView $ levelpuzzleCamera lvl
              view0 = View (a + 12.566) 0.785 80.0
              lvl' = levelpuzzleModifyGrid lvl $ \grid -> 
                      gridPushCameraCmds grid [ camcmdViewAdd t0 s0 view0 ]
          in  lvl' 

      t0 = 16.0
      s0 = 1.0 / rTF t0



iterationComplete' :: CompleteS -> Iteration LevelPuzzleWorld RunWorld
iterationComplete' s =
    defaultIteration s outputComplete' $ defaultStep doComplete 
                                       $ \s lvl run -> do

        -- game ends here!        
        return (setCamera lvl, run, [iterationComplete' s]) 

    where
      setCamera lvl = 
          if levelpuzzleCameraCmdsIsComplete lvl 
          then levelpuzzleSetCameraCmds lvl [ camcmdViewAdd t0 s0 (View 6.283 0.0 0.0) ]
          else lvl
      
      t0 = 8.0
      s0 = 1.0 / rTF t0




--------------------------------------------------------------------------------
--  iterationFailure

iterationFailure :: Iteration LevelPuzzleWorld RunWorld
iterationFailure = 
    makeIteration $ \lvl run -> do
        
        (lvl', run') <- outputFailure lvl run
       
        -- clear keys
        keysClear

        -- different states depending on Mode
        if levelpuzzleIsPuzzle lvl'
          then do s <- makeFailureSPuzzleMode (worldTick lvl')
                  iteration (iterationFailure' s) lvl' run'

          else do s <- makeFailureSLevelMode (worldTick lvl')
                  iteration (iterationFailure' s) lvl' run'



iterationFailure' :: FailureS -> Iteration LevelPuzzleWorld RunWorld
iterationFailure' s =
    defaultIteration s outputFailure' $ defaultStep doFailure $ \s lvl run -> do

        -- fixme: toggle LevelMode/PuzzleMode mode? 
        --        worldPushEvent run EventPuzzleMode/EventLevelMode
        keysTouchHandleButtonA (lvl, run, [iterationFailure' s]) (const (lvl, run, []))
