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
{-# LANGUAGE MultiParamTypeClasses #-}
module Game.LevelPuzzle.LevelPuzzleWorld
  (
    LevelPuzzleWorld (..),
    LevelPuzzleEvent (..),

    module Game.Grid,
    module Game.LevelPuzzle.LevelPuzzleWorld.Level,
    module Game.LevelPuzzle.LevelPuzzleWorld.Content,
    module Game.LevelPuzzle.LevelPuzzleWorld.RoomIx,
    module Game.LevelPuzzle.LevelPuzzleWorld.Room,
    module Game.LevelPuzzle.LevelPuzzleWorld.Wall,
    module Game.LevelPuzzle.LevelPuzzleWorld.Dot,

  ) where


import MyPrelude
import Game

import Game.Grid
import Game.LevelPuzzle.LevelPuzzleWorld.Level
import Game.LevelPuzzle.LevelPuzzleWorld.Content
import Game.LevelPuzzle.LevelPuzzleWorld.RoomIx
import Game.LevelPuzzle.LevelPuzzleWorld.Room
import Game.LevelPuzzle.LevelPuzzleWorld.Wall
import Game.LevelPuzzle.LevelPuzzleWorld.Dot
import Game.LevelPuzzle.LevelPuzzleWorld.OutputState



--------------------------------------------------------------------------------
--  LevelPuzzleWorld

data LevelPuzzleWorld =
    LevelPuzzleWorld
    {
        -- meta
        levelpuzzleCreator :: !String,
        levelpuzzleName:: !String,

        -- level
        levelpuzzleLevelIx :: !UInt,
        levelpuzzleLevel :: !Level,

        -- content
        levelpuzzleContent :: !Content,
        levelpuzzleOldContent :: !Content,
        
        -- ref 
        -- (this should ideally be a reference for the current level, not the old, but 
        --  that may cause round off errors with modelview (?))
        levelpuzzleRefOldContent :: !Segment,
        levelpuzzleRefSpace :: !Turn,

        -- state
        levelpuzzleIsPuzzle :: !Bool,
        levelpuzzleSegmentsCount :: !UInt,
        levelpuzzleIsComplete :: !Bool,
        levelpuzzleFailureEvent :: !LevelPuzzleEvent,
        levelpuzzleOutputState :: !OutputState,
        levelpuzzleEvents :: [LevelPuzzleEvent],

        -- file
        levelpuzzleFile :: !FilePath,
        levelpuzzleFileSize :: !UInt,
        levelpuzzleFileDefOffset :: !UInt,
        levelpuzzleFileDefNextOffset :: !UInt
    }


--------------------------------------------------------------------------------
--  LevelPuzzleEvent

data LevelPuzzleEvent =
    EventEmpty                      |
    EventNullSegments               |
    EventPathEatDotPlain DotPlain   |
    EventPathEatDotBonus DotBonus   |
    EventPathEatDotTele DotTele     |
    EventPathEatDotFinish DotFinish |
    EventPathEatWall Wall            



--------------------------------------------------------------------------------
--  instance World

instance World LevelPuzzleWorld LevelPuzzleEvent where
    worldTick =
        worldTick . contentGrid . levelpuzzleContent
    worldTickModify lvl f =
        levelpuzzleModifyContent lvl $ \cnt -> contentModifyGrid cnt $ \grid -> 
            worldTickModify grid f
    worldAllEvents =
        levelpuzzleEvents
    worldPushEvent lvl e =
        lvl { levelpuzzleEvents = levelpuzzleEvents lvl ++ [e] }


levelpuzzleModifyContent :: LevelPuzzleWorld -> (Content -> Content) -> LevelPuzzleWorld
levelpuzzleModifyContent lvl f =
    lvl { levelpuzzleContent = f (levelpuzzleContent lvl) }

contentModifyGrid :: Content -> (GridWorld -> GridWorld) -> Content
contentModifyGrid cnt f =
    cnt { contentGrid = f (contentGrid cnt) }


