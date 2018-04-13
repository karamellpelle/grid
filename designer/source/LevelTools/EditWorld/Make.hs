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
module LevelTools.EditWorld.Make
  (
    makeEditWorldNew,
    makeEditWorldLoad,
    makeEditWorldIO,

  ) where

import MyPrelude
import Game.Grid.GridWorld
import Game.Grid.Helpers
import Game.Grid.GridWorld.Make
import Game.LevelPuzzleMode.LevelPuzzleWorld
import Game.LevelPuzzleMode.Iteration
import LevelTools.EditWorld
import LevelTools.Make
import Game.MEnv
import File.Binary
import LevelTools.File



makeEditWorldNew = do
    let cnt = makeSemiContentEmpty
    grid <- makeGridWorldEmpty
    lvl <- makeLevelPuzzleWorldEmpty

    return EditWorld
        {
            editLevel = makeLevel "" False 0,
            editSemiContent = cnt,
            editLevelPuzzleEmpty = True,
            editLevelPuzzleWorld = lvl,
            editLevelPuzzleStack = [iterationBeginPlay],

            editFileOutput = "",
            editGrid = setCamera grid,
            editNode = mempty,
            editPushedNodes = [],
            editEditType = TypeDotPlain,
            editEvents = [],
            editCameraNode  = mempty,
            editMessage = mempty,

            editDotPlain = makeDotPlain mempty 1 1 0,
            editDotBonus = makeDotBonus mempty 1 1 1,
            editDotTele = makeDotTele mempty 1 1 mempty,
            editDotFinish = makeDotFinish mempty,
            editWall = makeWall False mempty (Node 1 0 0) (Node 0 1 0)

        }


makeEditWorldLoad path = do
    (level, srooms) <- io $ readBinary' rLevelFile path

    let cnt =  makeSemiContent srooms

    grid <- makeGridWorldEmpty
    lvl <- makeLevelPuzzleWorldEmpty

    return EditWorld
        {
            editLevel = level,
            editSemiContent = cnt,
            editLevelPuzzleEmpty = True,
            editLevelPuzzleWorld = lvl,
            editLevelPuzzleStack = [iterationBeginPlay],

            editFileOutput = "",
            editGrid = setCamera grid,
            editNode = mempty,
            editPushedNodes = [],
            editEditType = TypeDotPlain,
            editEvents = [],
            editCameraNode  = mempty,
            editMessage = mempty,

            editDotPlain = makeDotPlain mempty 1 1 0,
            editDotBonus = makeDotBonus mempty 1 1 1,
            editDotTele = makeDotTele mempty 1 1 mempty,
            editDotFinish = makeDotFinish mempty,
            editWall = makeWall False mempty (Node 1 0 0) (Node 0 1 0)

        }

    

setCamera grid = 
  gridModifyCamera grid $ \cam -> cameraSetView cam $ View 0.0 0.4 8.0


makeEditWorldIO path = do
    (level, srooms) <- io $ readBinary' rLevelFile path

    let cnt =  makeSemiContent srooms
        grid = error "no GridWordl in EditWorld"
        lvl  = error "no LevelWorld in EditWorld"

    return EditWorld
        {
            editLevel = level,
            editSemiContent = cnt,
            editLevelPuzzleEmpty = True,
            editLevelPuzzleWorld = lvl,
            editLevelPuzzleStack = [iterationBeginPlay],

            editFileOutput = "",
            editGrid = setCamera grid,
            editNode = mempty,
            editPushedNodes = [],
            editEditType = TypeDotPlain,
            editEvents = [],
            editCameraNode  = mempty,
            editMessage = mempty,

            editDotPlain = makeDotPlain mempty 1 1 0,
            editDotBonus = makeDotBonus mempty 1 1 1,
            editDotTele = makeDotTele mempty 1 1 mempty,
            editDotFinish = makeDotFinish mempty,
            editWall = makeWall False mempty (Node 1 0 0) (Node 0 1 0)

        }

