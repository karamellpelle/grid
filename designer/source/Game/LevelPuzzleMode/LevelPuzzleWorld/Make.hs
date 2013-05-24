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
module Game.LevelPuzzleMode.LevelPuzzleWorld.Make
  (
    loadLevelPuzzleWorld,
    unloadLevelPuzzleWorld,

  ) where


import MyPrelude
import System.IO
import File.Binary
import Game

import Game.LevelPuzzleMode.LevelPuzzleWorld
import Game.LevelPuzzleMode.LevelPuzzleWorld.OutputState
import Game.LevelPuzzleMode.File


-- | load LevelPuzzleWorld from path, starting at level ix
loadLevelPuzzleWorld :: FilePath -> UInt -> MEnv' LevelPuzzleWorld
loadLevelPuzzleWorld path ix = do
    
    (creator, name) <- io $ readBinary' rLevelPuzzleWorld path
    filesize <- io $ withBinaryFile path ReadMode $ \h -> fI `fmap` hFileSize h 
    off <- io $ readBinary' (rLevelOffset ix) path 

    if off == filesize

      -- make complete LevelPuzzleWorld 
      then do
        let level = makeLevelEmpty
        cnt <- makeContentEmpty
        cnt' <- makeContentEmpty
        state <- makeOutputState

        return LevelPuzzleWorld
               {
                  levelpuzzleCreator = creator,
                  levelpuzzleName = name,

                  levelpuzzleLevelIx = ix,
                  levelpuzzleLevel = level,
                  levelpuzzleOldContent = cnt,
                  levelpuzzleContent = cnt',
                 
                  levelpuzzleRefOldContent = mempty,
                  levelpuzzleRefSpace = mempty,
                  
                  levelpuzzleIsPuzzle = False,
                  levelpuzzleEvents = [],
                  levelpuzzleSegmentsCount = 0,
                  levelpuzzleIsComplete = True,
                  levelpuzzleOutputState = state,
                  levelpuzzleFailureEvent = EventEmpty,

                  levelpuzzleFile = path,
                  levelpuzzleFileSize = filesize,
                  levelpuzzleFileDefOffset = off,
                  levelpuzzleFileDefNextOffset = off
               }

      -- make LevelPuzzleWorld at level ix
      else do
        -- parse file
        (level, ixrooms, off') <- io $ readBinaryAt' rLevel path off
        
        -- make
        cnt <- makeContentEmpty
        cnt' <- makeContentCamera (levelSegments level) ixrooms $
                makeCameraWithView valueLevelPuzzleView
        state <- makeOutputState

        return LevelPuzzleWorld
               {
                  levelpuzzleCreator = creator,
                  levelpuzzleName = name,

                  levelpuzzleLevelIx = ix,
                  levelpuzzleLevel = level,
                  levelpuzzleOldContent = cnt,
                  levelpuzzleContent = cnt',
                  
                  levelpuzzleRefOldContent = mempty,
                  levelpuzzleRefSpace = mempty,
                  
                  levelpuzzleIsPuzzle = False,
                  levelpuzzleEvents = [],
                  levelpuzzleSegmentsCount = levelSegments level,
                  levelpuzzleIsComplete = False,
                  levelpuzzleOutputState = state,
                  levelpuzzleFailureEvent = EventEmpty,

                  levelpuzzleFile = path,
                  levelpuzzleFileSize = filesize,
                  levelpuzzleFileDefOffset = off,
                  levelpuzzleFileDefNextOffset = off'
               }


unloadLevelPuzzleWorld lvl = io $ do
    putStrLn $ "fixme: unloadLevelPuzzleWorld!!!"
