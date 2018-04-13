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
module Game.Run.Iteration.Memory
  (
    iterationMemoryMode,

  ) where


import MyPrelude
import Game

import Game.Run.RunWorld
import Game.Run.Do
import Game.Run.Output
import Game.Run.Scene
import Game.Run.Helpers
import Game.Run.Helpers.Make
import Game.Grid.Helpers
import Game.Memory.MemoryWorld

import Game.Run.Iteration.Iteration

import OpenGL
import OpenGL.Helpers




--------------------------------------------------------------------------------
--  

-- | begin iterate in MemoryMode
iterationMemoryMode :: Iteration' RunWorld
iterationMemoryMode = 
    makeIteration' $ \run -> do
        outputMemoryMode run

        iteration' (iterationMemoryMode' (runMemoryPeak run)) run


iterationMemoryMode' :: UInt -> Iteration' RunWorld
iterationMemoryMode' peak =
    modeIteration peak noOutput $ defaultStep doMemory $ \peak run b -> do

      -- handle Peak
      let mem = runMemoryWorld run
          peak' = max (runMemoryPeak run) (memoryLevelIx mem)
          run' = run { runMemoryPeak = peak' }
      
      -- look at MemoryWorld
      case runMemoryStack run' of

            -- MemoryMode finished. this should not happen, since we assume 
            -- an "infinite" (UInt) amount of levels :)
            []  -> do
                return (run', b, [])

            _   -> do

                -- handle escape
                sceneHandleEscape (runScene run)
                                  (return (run', b, [iterationMemoryMode' peak])) $ do

                    -- grab screenshot
                    run'' <- screenshotMemory run'

                    -- save RunWorld
                    saveRunWorld run''
                    
                    -- report score.
                    -- fixme: this causes the problem that score is only
                    --        reported if manually escaping from MemoryMode and
                    --        peak has strictly increased. but new peak is never
                    --        lost, since it is always written to RunWorld
                    unless (peak == peak') $ do
                        sendScore run''
                    
                    -- comment gameplay
                    run''' <- commentMemoryEscape run''

                    return (run''', b, [])

    where
      setMessage run str = 
          runMessageClearPush run str
      
      sendScore run = do
          let score = 2 * runMemoryPeak run + if runSpecialIsCompleted run
                                              then 1 else 0
          playersSendScore valuePlayersScoreMemoryMode (fI score)
          
