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
module Game.LevelPuzzle.Helpers.Make
  (
    loadLevelPuzzleWorld,
    levelpuzzleSetLevelNext,
    levelpuzzleSetLevelIx,

  ) where


import MyPrelude
import Game

import Game.LevelPuzzle.LevelPuzzleWorld
import Game.LevelPuzzle.LevelPuzzleWorld.OutputState
import Game.LevelPuzzle.Helpers
import Game.LevelPuzzle.File

import System.IO
import File.Binary



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
                  levelpuzzleFailureEvent = EventEmpty,
                  levelpuzzleOutputState = state,

                  levelpuzzleFile = path,
                  levelpuzzleFileSize = filesize,
                  levelpuzzleFileDefOffset = off,
                  levelpuzzleFileDefNextOffset = off
               }

      -- make LevelPuzzleWorld at level ix
      else do
        -- parse file
        (level, ixrooms, off') <- io $ readBinaryAt' rLevel path off
        cnt <- makeContentEmpty

        bonusAdd <- countBonusAdd (map snd ixrooms)
        cnt' <- makeContentCamera (levelSegments level + bonusAdd) ixrooms $ 
                                  makeCameraView valueLevelPuzzleGridView
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
                  levelpuzzleFailureEvent = EventEmpty,
                  levelpuzzleOutputState = state,

                  levelpuzzleFile = path,
                  levelpuzzleFileSize = filesize,
                  levelpuzzleFileDefOffset = off,
                  levelpuzzleFileDefNextOffset = off'
               }




--------------------------------------------------------------------------------
--  destroy

destroyLevelPuzzleWorld :: LevelPuzzleWorld -> MEnv' ()
destroyLevelPuzzleWorld lvl =
    io $ fixme "destroyLevelPuzzleWorld"



--------------------------------------------------------------------------------
--  

-- | load next level from levelpuzzleFileXXX
levelpuzzleSetLevelNext :: LevelPuzzleWorld -> MEnv' LevelPuzzleWorld
levelpuzzleSetLevelNext lvl = do

    -- next level exists?
    if levelpuzzleFileDefNextOffset lvl == levelpuzzleFileSize lvl

      -- complete
      then do
        destroyContent $ levelpuzzleOldContent lvl
        let cnt = levelpuzzleContent lvl
        cnt' <- makeContentEmpty
        return lvl
               {
                  levelpuzzleLevelIx = levelpuzzleLevelIx lvl + 1,

                  levelpuzzleOldContent = cnt,
                  levelpuzzleContent = cnt',

                  levelpuzzleRefOldContent = refOldContent lvl,
                  levelpuzzleRefSpace = refSpace lvl,

                  levelpuzzleIsComplete = True
               }

      else do
        -- parse file at offset 
        (level, ixrooms, off') <- io $ readBinaryAt' rLevel
                                                     (levelpuzzleFile lvl) 
                                                     (levelpuzzleFileDefNextOffset lvl)
        
        -- make Content
        destroyContent $ levelpuzzleOldContent lvl
        let cnt = levelpuzzleContent lvl

        bonusAdd <- countBonusAdd (map snd ixrooms)
        cnt' <- makeContentCamera (levelSegments level + bonusAdd) ixrooms $ 
                gridCamera $ contentGrid cnt

        return lvl
               {
                  levelpuzzleLevelIx = levelpuzzleLevelIx lvl + 1,
                  levelpuzzleLevel = level,

                  levelpuzzleOldContent = cnt,
                  levelpuzzleContent = contentCopySpeed cnt' cnt,

                  levelpuzzleRefOldContent = refOldContent lvl,
                  levelpuzzleRefSpace = refSpace lvl,

                  levelpuzzleSegmentsCount = levelSegments level,
                  levelpuzzleFileDefOffset = levelpuzzleFileDefNextOffset lvl,
                  levelpuzzleFileDefNextOffset = off'
               }
    
    where
      contentCopySpeed new old =
          let speed = pathSpeed $ gridPath $ contentGrid $ old
          in  contentModifyGrid new $ \grid -> gridModifyPath grid $ \path ->
                                path { pathSpeed = speed }


-- | load level ix from levelpuzzleFileXXX
levelpuzzleSetLevelIx :: LevelPuzzleWorld -> UInt -> MEnv' LevelPuzzleWorld
levelpuzzleSetLevelIx lvl ix = do
    off <- io $ readBinary' (rLevelOffset ix) (levelpuzzleFile lvl)
    if off == levelpuzzleFileSize lvl

      -- complete
      then do
        return lvl
               {
                  levelpuzzleIsComplete = True,
                  levelpuzzleLevelIx = ix
               }

      else do
        -- parse file
        (level, ixrooms, off') <- io $ readBinaryAt' rLevel (levelpuzzleFile lvl) off

        -- make
        destroyContent $ levelpuzzleContent lvl
        destroyContent $ levelpuzzleOldContent lvl
        cnt <- makeContentEmpty

        bonusAdd <- countBonusAdd (map snd ixrooms)
        cnt' <- makeContentCamera (levelSegments level + bonusAdd) ixrooms $
                makeCameraView valueLevelPuzzleGridView
        return lvl
               {
                  levelpuzzleLevelIx = ix,
                  levelpuzzleLevel = level,

                  levelpuzzleOldContent = cnt,
                  levelpuzzleContent = cnt',

                  levelpuzzleRefOldContent = refOldContent lvl,
                  levelpuzzleRefSpace = refSpace lvl,

                  levelpuzzleIsComplete = False,

                  levelpuzzleEvents = [],

                  levelpuzzleSegmentsCount = levelSegments level,
                  levelpuzzleFileDefOffset = off, 
                  levelpuzzleFileDefNextOffset = off'
               }




--------------------------------------------------------------------------------
--  

countBonusAdd :: [Room] -> MEnv' UInt
countBonusAdd rooms = io $ do
    add <- helper 0 rooms
    return add
    where
      helper n [] =
         return n
      helper n (r:rs) = do
          let arr = roomDotBonus r
              size = roomDotBonusSize r
          n' <- foldM (step arr) n (range 0 size)
          helper n' rs

      step arr = \n ix -> do
          dot <- dotbonusarrayAt arr ix
          return $ n + dotbonusCount dot * dotbonusAdd dot

refOldContent :: LevelPuzzleWorld -> Segment
refOldContent lvl = 
    segmentInverse $ pathCurrent $ levelpuzzlePath lvl

refSpace :: LevelPuzzleWorld -> Turn
refSpace lvl = 
    levelpuzzleRefSpace lvl `mappend` (turnInverse $ pathTurn $ levelpuzzlePath lvl)




