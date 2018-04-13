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
module Game.Run.Helpers.World
  (
    runModifyLevelPuzzleWorld,
    runLevelPuzzleIx,
    runLevelPuzzleSetIx,
    
    runMemoryIx,
    runMemorySetIx,

  ) where


import MyPrelude
import File
import Game
import Game.Run.RunWorld
import Game.Run.RunData
import Game.Grid.Helpers
import Game.LevelPuzzle.Helpers
import Game.LevelPuzzle.Helpers.Make
import Game.LevelPuzzle.Iteration             as LevelPuzzle
import Game.Memory.Iteration                  as Memory
import Game.Memory.Helpers.Make




runModifyLevelPuzzleWorld :: RunWorld -> (LevelPuzzleWorld -> LevelPuzzleWorld) -> 
                             RunWorld
runModifyLevelPuzzleWorld run f =
    run { runLevelPuzzleWorld = f $ runLevelPuzzleWorld run }


--------------------------------------------------------------------------------
--  

runLevelPuzzleIx :: RunWorld -> UInt
runLevelPuzzleIx run =
    levelpuzzleLevelIx $ runLevelPuzzleWorld run


runMemoryIx :: RunWorld -> UInt 
runMemoryIx run =
    memoryLevelIx $ runMemoryWorld run


runLevelPuzzleSetIx :: RunWorld -> UInt -> MEnv' RunWorld
runLevelPuzzleSetIx run ix = do
    lvl' <- levelpuzzleSetLevelIx (runLevelPuzzleWorld run) ix

    -- set face tex
    path <- io $ fileStaticData "Run/Output/levelpuzzle_tex.png"
    screenshotLevelPuzzle <- remakeScreenshot path $ runLevelPuzzleScreenshot run
    
    return run 
           {
              runLevelPuzzleWorld = lvl',
              runLevelPuzzleStack = [ LevelPuzzle.iterationBeginPlay ],
              runLevelPuzzleScreenshot = screenshotLevelPuzzle
           }



runMemorySetIx :: RunWorld -> UInt -> MEnv' RunWorld
runMemorySetIx run ix = do

    -- make new MemoryWorld
    -- fixme: reload, to save views
    destroyMemoryWorld $ runMemoryWorld run
    mem <- makeMemoryWorld ix
    
    -- set face tex
    path <- io $ fileStaticData "Run/Output/memory_tex.png"
    screenshotMemory <- remakeScreenshot path (runMemoryScreenshot run)

    return run 
           {
              runMemoryWorld = mem,
              runMemoryStack = [ Memory.iterationBeginShow ],
              runMemoryScreenshot = screenshotMemory
           }



