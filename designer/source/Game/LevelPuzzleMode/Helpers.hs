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
module Game.LevelPuzzleMode.Helpers
  (
    levelpuzzleClearEvents,
    levelpuzzleModifyContent,
    levelpuzzleModifyGrid,
    levelpuzzleModifyCamera,

    levelpuzzleCamera,
    levelpuzzleGrid,
    levelpuzzlePath,

    levelpuzzleIsSpecial,
    levelpuzzleLevelPuzzleTag,
    levelpuzzleLevelIx,
    levelpuzzleSetLevelNext,
    levelpuzzleSetLevelIx,
    levelpuzzleBeginLevel,

    contentIsEmpty,

  ) where

import MyPrelude
import File
import File.Binary
import Game
import Game.Grid.Helpers
import Game.LevelPuzzleMode.LevelPuzzleWorld
import Game.LevelPuzzleMode.File


--------------------------------------------------------------------------------
--  


levelpuzzleClearEvents :: LevelPuzzleWorld -> LevelPuzzleWorld
levelpuzzleClearEvents lvl =
    lvl { levelpuzzleEvents = [] }


levelpuzzleGrid :: LevelPuzzleWorld -> GridWorld
levelpuzzleGrid = 
    contentGrid . levelpuzzleContent

levelpuzzlePath :: LevelPuzzleWorld -> Path
levelpuzzlePath = 
    gridPathA . levelpuzzleGrid

levelpuzzleCamera :: LevelPuzzleWorld -> Camera
levelpuzzleCamera =
    gridCamera . levelpuzzleGrid


levelpuzzleModifyContent :: LevelPuzzleWorld -> (Content -> Content) -> LevelPuzzleWorld
levelpuzzleModifyContent lvl f =
    lvl { levelpuzzleContent = f (levelpuzzleContent lvl) }


levelpuzzleModifyGrid :: LevelPuzzleWorld -> (GridWorld -> GridWorld) -> LevelPuzzleWorld
levelpuzzleModifyGrid lvl f =
    levelpuzzleModifyContent lvl $ \cnt -> contentModifyGrid cnt f


levelpuzzleModifyCamera :: LevelPuzzleWorld -> (Camera -> Camera) -> LevelPuzzleWorld
levelpuzzleModifyCamera lvl f =
    levelpuzzleModifyGrid lvl $ \grid -> gridModifyCamera grid f


--------------------------------------------------------------------------------
--  Level

levelpuzzleLevelPuzzleTag :: LevelPuzzleWorld -> Bool
levelpuzzleLevelPuzzleTag =
    levelPuzzleTag . levelpuzzleLevel




--------------------------------------------------------------------------------
--  

-- | there is a special LevelPuzzleWorld
levelpuzzleIsSpecial :: LevelPuzzleWorld -> Bool
levelpuzzleIsSpecial lvl = 
    takeFileName (levelpuzzleFile lvl) == valueFileNameSpecialLevelPuzzleWorld
    --takeFileName (levelpuzzleFile lvl) == ""



--------------------------------------------------------------------------------
--  load


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
        
        -- make
        destroyContent $ levelpuzzleOldContent lvl
        let cnt = levelpuzzleContent lvl
        cnt' <- makeContentCamera (levelSegments level) ixrooms $ 
                gridCamera $ contentGrid cnt
        return lvl
               {
                  levelpuzzleLevelIx = levelpuzzleLevelIx lvl + 1,
                  levelpuzzleLevel = level,

                  levelpuzzleOldContent = cnt,
                  levelpuzzleContent = cnt',

                  levelpuzzleRefOldContent = refOldContent lvl,
                  levelpuzzleRefSpace = refSpace lvl,

                  levelpuzzleSegmentsCount = levelSegments level,
                  levelpuzzleFileDefOffset = levelpuzzleFileDefNextOffset lvl,
                  levelpuzzleFileDefNextOffset = off'
               }



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
        cnt' <- makeContentCamera (levelSegments level) ixrooms $
                makeCameraWithView valueLevelPuzzleView
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


refOldContent :: LevelPuzzleWorld -> Segment
refOldContent lvl = 
    segmentInverse $ pathCurrent $ levelpuzzlePath lvl

refSpace :: LevelPuzzleWorld -> Turn
refSpace lvl = 
    levelpuzzleRefSpace lvl `mappend` (turnInverse $ pathTurn $ levelpuzzlePath lvl)


--------------------------------------------------------------------------------
--  begin

levelpuzzleBeginLevel :: LevelPuzzleWorld -> MEnv' LevelPuzzleWorld
levelpuzzleBeginLevel lvl = do
    cnt' <- contentBegin $ levelpuzzleContent lvl
    return lvl
           {
              levelpuzzleContent = cnt',
              levelpuzzleEvents = [],
              levelpuzzleSegmentsCount = levelSegments (levelpuzzleLevel lvl)
           }



--------------------------------------------------------------------------------
--  Content

contentIsEmpty :: Content -> Bool
contentIsEmpty cnt =
    contentRoomsSize cnt == 0



contentModifyGrid :: Content -> (GridWorld -> GridWorld) -> Content
contentModifyGrid cnt f =
    cnt { contentGrid = f (contentGrid cnt) }


contentBegin :: Content -> MEnv' Content
contentBegin cnt = do
    roomsBegin (contentRoomsSize cnt) (contentRooms cnt) 
    grid' <- gridBegin $ contentGrid cnt
    return cnt
           {
              contentGrid = grid',
              contentRoom = 0,
              contentEatRoom = 0,
              contentEatTick = 0.0,
              contentEatPathBegin = 0

           }
    where
      gridBegin grid = do
          path' <- pathClear $ gridPathA grid
          return $ gridModifyCamera (grid { gridPathA = path' }) $ \cam -> 
                   cameraSetPath cam path'

      roomsBegin size rooms = io $
          roomarrayUpdateIO size rooms $ \room -> do
              dotplainarrayUpdate (roomDotPlainSize room) (roomDotPlain room) $ \dot -> 
                  dot { dotplainCount = dotplainSize dot }
              dotbonusarrayUpdate (roomDotBonusSize room) (roomDotBonus room) $ \dot -> 
                  dot { dotbonusCount = dotbonusSize dot }
              dottelearrayUpdate (roomDotTeleSize room) (roomDotTele room) $ \dot -> 
                  dot { dotteleCount = dotteleSize dot }
              dottelearrayUpdate (roomDotTeleSize room) (roomDotTele room) $ \dot -> 
                  dot { dotteleCount = dotteleSize dot }
              return room { roomPathBeginEnd = [] }


