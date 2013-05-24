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
module Game.Run.Do
  (
    doEmpty,
    doMain,
    doLevelPuzzle,
    doMemory,
    doLevelPuzzleComplete,

  ) where

import MyPrelude
import Game

import Game.Grid
import Game.Run.RunWorld
import Game.Run.Helpers
import Game.Run.Do.Input
import Game.Run.Do.Grid
import Game.Run.Eggs.Do





modifyBegin :: RunWorld -> MEnv' RunWorld
modifyBegin run = do
    return $ runClearEvents run


--------------------------------------------------------------------------------
--  

doEmpty :: s -> RunWorld -> b -> MEnv' (s, RunWorld, b) 
doEmpty = \s run b -> do

    -- begin modify
    run' <- modifyBegin run 

    -- do GridWorld
    (s', grid', run'') <- doGrid s (runGrid run') run'

    return (s', run'' { runGrid = grid' }, b)


doMain :: s -> RunWorld -> b -> MEnv' (s, RunWorld, b)
doMain = \s run b -> do

    -- begin modify
    run' <- modifyBegin run

    -- control cube
    run'' <- inputRunTurnFace run'
    
    -- do GridWorld
    (s', grid', run''') <- doGridCamera s (runGrid run'') run''
    let run'''' = run''' { runGrid = grid' }

    -- do EasterEggs
    run''''' <- doEggs run''''
  
    return (s', run''''', b) 




doLevelPuzzle :: s -> RunWorld -> b -> MEnv' (s, RunWorld, b) 
doLevelPuzzle = \s run b -> do

    -- begin modify
    run' <- modifyBegin run

    -- iterate LevelPuzzleWorld
    (lvl', run'', stack') <- iterateABStack (runLevelPuzzleWorld run') run' 
                                            (runLevelPuzzleStack run')
    let run''' = run'' 
                 {
                    runLevelPuzzleWorld = lvl',
                    runLevelPuzzleStack = stack'
                 }

    return (s, run''', b)



doMemory :: s -> RunWorld -> b -> MEnv' (s, RunWorld, b) 
doMemory = \s run b -> do

    -- begin modify
    run' <- modifyBegin run

    -- iterate MemoryWorld
    (mem', run'', stack') <- iterateABStack (runMemoryWorld run') run' 
                                            (runMemoryStack run')
    let run''' = run''
                 {
                    runMemoryWorld = mem',
                    runMemoryStack = stack'
                 }

    return (s, run''', b)



doLevelPuzzleComplete :: s -> RunWorld -> b -> MEnv' (s, RunWorld, b)
doLevelPuzzleComplete = \s run b -> do
    
    -- begin modify
    run' <- modifyBegin run

    -- do GridWorld
    (s', grid', run'') <- doGrid s (runGrid run') run'
    let run''' = run'' { runGrid = grid' }

    -- do EasterEggs
    run'''' <- doEggs run'''
  
    return (s', run'''', b) 




