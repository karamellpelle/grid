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

module Game.LevelPuzzleMode.LevelPuzzleWorld
  (
    LevelPuzzleWorld (..),
    LevelPuzzleEvent (..),

    module Game.Grid.GridWorld,
    module Game.LevelPuzzleMode.LevelPuzzleWorld.Level,
    module Game.LevelPuzzleMode.LevelPuzzleWorld.Content,

  ) where


import MyPrelude
import Game.World
import Game.Grid.GridWorld
import Game.LevelPuzzleMode.LevelPuzzleWorld.Level
import Game.LevelPuzzleMode.LevelPuzzleWorld.Content
import Game.LevelPuzzleMode.LevelPuzzleWorld.OutputState



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
        -- (ideally (RefWorld :: Segment) and draw levels relative to that, but
        --  i think this may cause round off errors with modelview)
        levelpuzzleRefOldContent :: !Segment,
        levelpuzzleRefSpace :: !Turn,

        -- state
        levelpuzzleIsPuzzle :: !Bool,
        levelpuzzleEvents :: [LevelPuzzleEvent],
        levelpuzzleSegmentsCount :: !UInt,
        levelpuzzleIsComplete :: !Bool,
        levelpuzzleOutputState :: !OutputState,
        levelpuzzleFailureEvent :: !LevelPuzzleEvent,

        -- file
        levelpuzzleFile :: !FilePath,
        levelpuzzleFileSize :: !UInt,
        levelpuzzleFileDefOffset :: !UInt,
        levelpuzzleFileDefNextOffset :: !UInt
    }



data LevelPuzzleEvent =
    EventEmpty              |
    EventNullSegments       |
    EventPathEatDotFinish   |
    EventPathEatWall



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



--------------------------------------------------------------------------------
--  

levelpuzzleModifyContent :: LevelPuzzleWorld -> (Content -> Content) -> 
                            LevelPuzzleWorld
levelpuzzleModifyContent lvl f =
    lvl { levelpuzzleContent = f (levelpuzzleContent lvl) }

contentModifyGrid :: Content -> (GridWorld -> GridWorld) -> Content
contentModifyGrid cnt f =
    cnt { contentGrid = f (contentGrid cnt) }


