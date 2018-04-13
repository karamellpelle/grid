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
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses  #-}
module LevelTools.EditWorld
  (
    EditWorld (..),
    EditEvent (..),

    module Game.LevelPuzzleMode.LevelPuzzleWorld,
    module LevelTools.SemiContent,
    module LevelTools.SemiRoom,
    module LevelTools.EditType,

  ) where

import MyPrelude
import Game

import Game.LevelPuzzleMode.LevelPuzzleWorld
import Game.Run.RunWorld
import LevelTools.SemiContent
import LevelTools.SemiRoom
import LevelTools.EditType


data EditWorld =
    EditWorld
    {
        editLevel :: !Level,
        editSemiContent :: SemiContent,
        
        editLevelPuzzleEmpty :: !Bool,
        editLevelPuzzleWorld :: LevelPuzzleWorld,
        editLevelPuzzleStack :: !(IterationStack LevelPuzzleWorld RunWorld),
        
        editFileOutput :: !FilePath,
        editGrid :: GridWorld,
        editNode :: !Node,
        editPushedNodes :: ![Node],
        editEditType :: !EditType,
        editEvents :: [EditEvent],
        editCameraNode :: !Node,
        editMessage :: String,

        editDotPlain :: DotPlain,
        editDotBonus :: DotBonus,
        editDotTele :: DotTele,
        editDotFinish :: DotFinish,
        editWall :: Wall
    }



data EditEvent

--------------------------------------------------------------------------------
--  helpers

instance World EditWorld EditEvent where
    worldTick =
        worldTick . editGrid
    worldTickModify edit f =
        modifyG edit $ \grid -> worldTickModify grid f
    worldAllEvents = 
        editEvents 
    worldPushEvent edit e =
        edit { editEvents  = editEvents edit ++ [e] }

modifyG edit f =
    edit { editGrid = f (editGrid edit) }

