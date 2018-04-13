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
module Game.Run.Helpers.Make
  (
    loadRunWorld,
    reloadRunWorld,
    saveRunWorld,

  ) where

import MyPrelude
import Game

import Game.Run.RunWorld
import Game.Run.RunWorld.OutputState
import Game.Run.Helpers
import Game.Run.File

import Game.Grid
import Game.LevelPuzzle           as LP
import Game.LevelPuzzle.Iteration as LP
import Game.Memory                as M
import Game.Memory.Iteration      as M

import File
import File.Binary

import OpenGL
import OpenGL.Helpers


--------------------------------------------------------------------------------
--  load 


-- | load RunWorld from file
loadRunWorld :: FilePath -> MEnv' RunWorld
loadRunWorld path = do
    (lpname, lpix, mix, spec, intensity) <- io $ readBinary' rRunWorld path
    
    -- Grid
    grid <- makeRunGrid

    -- LevelWorld
    lppath <- fileLevelPuzzleWorld lpname
    lvl <- loadLevelPuzzleWorld lppath lpix

    -- MemoryWorld
    mem <- makeMemoryWorld mix 

    -- Scene
    scene <- makeSceneEmpty 

    -- screenshots
    ssLevelPuzzle <- asScreenshot "Run/Output/levelpuzzle_tex.png"
    ssMemory <- asScreenshot "Run/Output/memory_tex.png"
    ssForeign <- asScreenshot "Run/Output/foreign_tex.png"
    
    -- OutputState
    state <- makeOutputState

    return  RunWorld
            { 
                -- LevelPuzzle
                runLevelPuzzleFileName = takeFileName lppath,
                runLevelPuzzlePeak = lpix,
                runLevelPuzzleWorld = lvl,
                runLevelPuzzleStack = [LP.iterationBeginPlay],
                runLevelPuzzleScreenshot = ssLevelPuzzle,
                
                -- Memory
                runMemoryPeak = mix,
                runMemoryWorld = mem,
                runMemoryStack = [M.iterationBeginShow],
                runMemoryScreenshot = ssMemory,
              
                -- Foreign
                runForeignScreenshot = ssForeign,

                -- Scene
                runScene = scene,

                runFile = path,
                runIntensity = intensity,
                runTurn = turnFaceLevelMode,
                runGrid = grid,
                runEvents = [],
                runMessage = makeRunMessage,
                runSpecialIsCompleted = spec,
                
                runOutputState = state, 

                -- Eggs
                runEggKonami = eggKonami,
                runEggMessage = eggMessage

            } 


    where
      asScreenshot path = do
          path' <- io $ fileStaticData path
          makeScreenshot path'

      makeRunGrid = do
          grid <- makeGridWorld valueRunMessageSize 
          return $ gridModifyPath grid $ \path -> path 
                            {
                                pathCurrent = dummySegment,
                                pathSpeed = valueRunMessageSpeed 
                            }
     
      makeRunMessage =
          makeMessage mempty mempty




-- | update RunWorld with content of file
reloadRunWorld :: RunWorld -> FilePath -> MEnv' RunWorld
reloadRunWorld run path = do
    (lpname, lpix, mix, spec, intensity) <- io $ readBinary' rRunWorld path
    
    -- LevelWorld
    lvl <- fileLevelPuzzleWorld lpname >>= \path -> 
           loadLevelPuzzleWorld path lpix

    -- MemoryWorld
    mem <- makeMemoryWorld mix 

    -- Screenshot
    ssLevelPuzzle <- asScreenshot (runLevelPuzzleScreenshot run) 
                     "Run/Output/levelpuzzle_tex.png"
    ssMemory <- asScreenshot (runMemoryScreenshot run)
                "Run/Output/memory_tex.png"
    ssForeign <- asScreenshot (runForeignScreenshot run)
                 "Run/Output/foreign_tex.png"
    
    return  run
            { 
                -- LevelPuzzle
                runLevelPuzzleFileName = lpname,
                runLevelPuzzlePeak = lpix,
                runLevelPuzzleWorld = lvl,
                runLevelPuzzleStack = [LP.iterationBeginPlay],
                runLevelPuzzleScreenshot = ssLevelPuzzle,
                
                -- Memory
                runMemoryPeak = mix,
                runMemoryWorld = mem,
                runMemoryStack = [M.iterationBeginShow],
                runMemoryScreenshot = ssMemory,
              
                -- Foreign
                runForeignScreenshot = ssForeign,

                runFile = path,
                runIntensity = intensity,
                runSpecialIsCompleted = spec,
                runEvents = [] -- ?

            } 

    where
      asScreenshot ss path = do
          path' <- io $ fileStaticData path
          remakeScreenshot path' ss
      

--------------------------------------------------------------------------------
--  save

saveRunWorld :: RunWorld -> MEnv' ()
saveRunWorld run = io $ do 
    -- read map LevelPuzzleName -> Peak for run world
    namespeaks <- readBinary' rLevelPuzzleNamePeakS $ runFile run

    -- write to tmp file
    tmp <- fileTmp "runworld.tmp"
    writeBinary' (wRunWorld namespeaks run) tmp

    -- copy tmp file to run file, then remove tmp file
    copyFile tmp $ runFile run
    removeFile tmp


