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
module LevelTools.EditWorld.Constructs
  (
    emptyEditWorld,
    loadEditWorld,
    saveEditWorld,

    makeLevelWorld,
    makePuzzleWorld,

  ) where

import MyPrelude
import Game.Grid.GridWorld
import Game.Grid.GridWorld.Helpers
import Game.Grid.GridWorld.Constructs
import Game.LevelMode.LevelDef
import Game.LevelMode.LevelDef.LoadSave
import LevelTools.EditWorld
import Data.List
import Game.Values



emptyEditWorld =
    EditWorld
    {
        editLevelWalls = [],
        editLevelDots = [],
        editLevelName = "",
        editLevelMaxSegments = 0,

        editPathOut = "",
        editGrid = modifyCamera emptyGridWorld $ \camera -> 
                   camera { cameraViewIdeal = valueModeGridView },
        editEatOrd = 0,
        editIsEatOrd = True,
        editNode = mempty,
        editPushedNodes = [],
        editType = TypeSimpleDot,
        editTypeValue = 0
    }

        




loadEditWorld path = do
    def <- loadLevelDef path
    return $ makeEditWorld def


saveEditWorld edit path = do
    saveLevelDef (makeLevelDef edit) path

makeEditWorld def =
    EditWorld
    {
        editLevelWalls = leveldefWalls def,
        editLevelDots = leveldefDots def ++ concat (leveldefDotsOrd def),
        editLevelName = leveldefName def,
        editLevelMaxSegments = leveldefMaxSegments def,

        editPathOut = "",
        editGrid = modifyCamera emptyGridWorld $ \camera -> 
                   camera { cameraViewIdeal = valueModeGridView },
        editEatOrd = 0,
        editIsEatOrd = True,
        editNode = mempty,
        editPushedNodes = [],
        editType = TypeSimpleDot,
        editTypeValue = 0
    }
 

makeLevelDef edit =
    let (dots, dotss) = helper $ editLevelDots edit
    in  LevelDef
    {
        leveldefName = editLevelName edit,
        leveldefMaxSegments = editLevelMaxSegments edit,

        leveldefWalls = editLevelWalls edit,
        leveldefDots = dots,
        leveldefDotsOrd = dotss
    }

    where
        helper dots = 
            let (dots0, dots1) = partition (isNothing . dotEatOrd) dots
                dots' = dots0
                dotss' = groupBy myGroup $ sortBy mySort dots1
            in  (dots', dotss')

        mySort dot0 dot1 = 
            let Just ord0 = dotEatOrd dot0
                Just ord1 = dotEatOrd dot1
            in  compare ord0 ord1

        myGroup dot0 dot1 = 
            let Just ord0 = dotEatOrd dot0
                Just ord1 = dotEatOrd dot1
            in  ord0 == ord1



makePuzzleWorld =
    makeLevelWorld


makeLevelWorld edit = 
    LevelWorld
    {
        levelLevelIx = 0,
        levelLevelCurrent = makeLevelDef edit,
        levelLevelNexts = [],

        levelOldPhysical = myEmptyPhysical,
        levelOldOrigin = mempty,
        levelOldPathEatOrd = 0,

        levelPhysical = myEmptyPhysical,
        levelPathEatOrd = 0,
        levelSegmentsRemaining = 0,
        levelEvents = []
    }



myEmptyPhysical = 
    let grid = emptyGridWorld
        grid' = modifyCamera grid $ \camera -> camera { cameraView = valueModeGridView,
                                                        cameraViewIdeal = valueModeGridView }

    in  Physical
        {
            physicalGrid = grid',
            physicalWalls = [],
            physicalDots = []
        }



