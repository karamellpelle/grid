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
module Game.LevelPuzzle.Helpers
  (
    levelpuzzleCameraCmdsIsComplete,
    levelpuzzleSetCameraCmds,
    levelpuzzlePushCameraCmds,

    levelpuzzleModifyContent,
    levelpuzzleModifyGrid,
    levelpuzzleModifyCamera,
    levelpuzzleModifyPath,

    levelpuzzleCamera,
    levelpuzzleGrid,
    levelpuzzlePath,

    levelpuzzleClearEvents,
    levelpuzzleIsSpecial,
    levelpuzzleLevelPuzzleTag,
    levelpuzzleLevelIx,
    levelpuzzleBeginLevel,

    contentIsEmpty,
    contentModifyGrid,

  ) where

import MyPrelude
import File
import File.Binary
import Game
import Game.Grid.Helpers
import Game.LevelPuzzle.LevelPuzzleWorld
import Game.LevelPuzzle.File


--------------------------------------------------------------------------------
--  

levelpuzzleCameraCmdsIsComplete :: LevelPuzzleWorld -> Bool
levelpuzzleCameraCmdsIsComplete = 
    gridCameraCmdsIsComplete . levelpuzzleGrid 

levelpuzzleSetCameraCmds :: LevelPuzzleWorld -> [CameraCommand] -> LevelPuzzleWorld
levelpuzzleSetCameraCmds lvl cmds =
    levelpuzzleModifyGrid lvl $ \grid -> gridSetCameraCmds grid cmds

levelpuzzlePushCameraCmds :: LevelPuzzleWorld -> [CameraCommand] -> LevelPuzzleWorld
levelpuzzlePushCameraCmds lvl cmds =
    levelpuzzleModifyGrid lvl $ \grid -> gridPushCameraCmds grid cmds


levelpuzzleClearEvents :: LevelPuzzleWorld -> LevelPuzzleWorld
levelpuzzleClearEvents lvl =
    lvl { levelpuzzleEvents = [] }


levelpuzzleGrid :: LevelPuzzleWorld -> GridWorld
levelpuzzleGrid = 
    contentGrid . levelpuzzleContent

levelpuzzlePath :: LevelPuzzleWorld -> Path
levelpuzzlePath = 
    gridPath . levelpuzzleGrid

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


levelpuzzleModifyPath :: LevelPuzzleWorld -> (Path -> Path) -> LevelPuzzleWorld
levelpuzzleModifyPath lvl f =
    levelpuzzleModifyGrid lvl $ \grid -> gridModifyPath grid f


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
    gridIsEmpty $ contentGrid cnt



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
              contentEatPathBegin = 0,
              contentEatRoom = 0,
              contentEatTick = 0.0

           }
    where
      gridBegin grid = do
          path' <- pathClear $ gridPath grid
          return $ gridModifyCamera (grid { gridPath = path' }) $ \cam -> 
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


