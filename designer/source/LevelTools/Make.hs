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
module LevelTools.Make
  (
    lvlFromEdit,
    makeLevelPuzzleWorldEmpty,
    writeEditAsLevel,
    writeWorldHeader,

  ) where
import MyPrelude
import Game
import LevelTools.EditWorld
import LevelTools.File
import Game.LevelPuzzleMode.LevelPuzzleWorld
import Game.LevelPuzzleMode.LevelPuzzleWorld.OutputState
import Game.Grid.GridWorld
import File.Binary

lvlFromEdit :: EditWorld -> MEnv' LevelPuzzleWorld
lvlFromEdit edit = do
    let level = editLevel edit
        creator = "grid_design"
        name = "unknown name"
        path = "/dev/null"
    cnt <- makeContentEmpty
    ixrooms <- io $ mapM makeRoomIxRoom $ scontentRooms $ editSemiContent edit
    cnt' <- makeContentCamera (levelSegments level) ixrooms $ 
            makeCameraWithView valueLevelPuzzleView
    state <- makeOutputState 
    return LevelPuzzleWorld
           {
              levelpuzzleCreator = creator,
              levelpuzzleName = name,

              levelpuzzleLevelIx = 0,
              levelpuzzleLevel = level,
              levelpuzzleOldContent = cnt,
              levelpuzzleContent = cnt',
              
              levelpuzzleRefOldContent = mempty,
              levelpuzzleRefSpace = mempty,
              
              levelpuzzleIsPuzzle = False,
              levelpuzzleEvents = [],
              levelpuzzleSegmentsCount = levelSegments level,
              levelpuzzleIsComplete = False,
              levelpuzzleFailureEvent = EventEmpty,

              levelpuzzleFile = path,
              levelpuzzleFileSize = 0,
              levelpuzzleFileDefOffset = 0,
              levelpuzzleFileDefNextOffset = 0,
              levelpuzzleOutputState  = state
           }


makeLevelPuzzleWorldEmpty :: MEnv' LevelPuzzleWorld
makeLevelPuzzleWorldEmpty = do
    let level = makeLevel "empty_level" False 0
        creator = "empty_creator"
        name = "empty_name"
        path = "/dev/null"
    cnt <- makeContentEmpty
    cnt' <- makeContentEmpty
    
    state <- makeOutputState 
    return LevelPuzzleWorld
           {
              levelpuzzleCreator = creator,
              levelpuzzleName = name,

              levelpuzzleLevelIx = 0,
              levelpuzzleLevel = level,
              levelpuzzleOldContent = cnt,
              levelpuzzleContent = cnt',
              
              levelpuzzleRefOldContent = mempty,
              levelpuzzleRefSpace = mempty,
              
              levelpuzzleIsPuzzle = False,
              levelpuzzleEvents = [],
              levelpuzzleSegmentsCount = levelSegments level,
              levelpuzzleIsComplete = False,
              levelpuzzleFailureEvent = EventEmpty,

              levelpuzzleFile = path,
              levelpuzzleFileSize = 0,
              levelpuzzleFileDefOffset = 0,
              levelpuzzleFileDefNextOffset = 0,
              levelpuzzleOutputState  = state
           }




writeEditAsLevel :: EditWorld -> FilePath -> IO ()
writeEditAsLevel edit path = do
    maybeWrite <- writeBinary (wLevel edit) path
    case maybeWrite of
        Nothing   -> return ()
        Just msg  -> putStrLn $ "could not write edit to file " ++ path ++ ": \n" ++ msg


writeWorldHeader :: String -> String -> FilePath -> IO ()
writeWorldHeader creator name path = do
    maybeWrite <- writeBinary (wWorldHeader creator name) path
    case maybeWrite of
        Nothing   -> return ()
        Just msg  -> putStrLn $ "could not write world header to file " ++ path ++ ": \n" ++ msg

    
