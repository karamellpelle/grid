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
module Game.Run.RunWorld
  (
    RunWorld (..),
    RunEvent (..),

    module Game.LevelPuzzle,
    module Game.Memory,
    module Game.Grid,

    module Game.Run.RunWorld.Message,
    module Game.Run.RunWorld.Scene,
    module Game.Run.RunWorld.Face,
    module Game.Run.RunWorld.Screenshot,
    module Game.Run.Eggs,

    
  ) where

import MyPrelude
import Game

import Game.Run.RunWorld.Message
import Game.Run.RunWorld.Scene
import Game.Run.RunWorld.Face
import Game.Run.RunWorld.Screenshot
import Game.Run.Eggs

import Game.Grid
import Game.LevelPuzzle
import Game.Memory

import Game.Run.RunWorld.OutputState


data RunWorld =
    RunWorld
    {  

        -- LevelPuzzle
        runLevelPuzzleFileName :: !String,
        runLevelPuzzlePeak :: !UInt,
        runLevelPuzzleWorld :: !LevelPuzzleWorld,
        runLevelPuzzleStack :: !(IterationStack LevelPuzzleWorld RunWorld),
        runLevelPuzzleScreenshot :: !Screenshot,

        -- Memory
        runMemoryPeak :: !UInt,
        runMemoryWorld :: !MemoryWorld,
        runMemoryStack :: !(IterationStack MemoryWorld RunWorld),
        runMemoryScreenshot :: !Screenshot,

        -- Foreign
        runForeignScreenshot :: !Screenshot,

        -- Scene
        runScene :: !Scene,

        runFile :: !FilePath,
        runIntensity :: !Float,
        runGrid :: !GridWorld,
        runMessage :: !Message, 
        runTurn :: !Turn,
        runEvents :: ![RunEvent],
        runSpecialIsCompleted :: !Bool,
        
        -- todo:
        --runMyPaths :: !(),                 -- ^ fixme: list of my paths (from file)
        --runFriendPaths :: [LevelPath],    -- ^ paths from friends
        --runFriendPathIx :: !LevelIx,       -- ^ we want friend paths for this level
        
        -- Easter Eggs!
        runEggKonami :: !EggKonami,
        runEggMessage :: !EggMessage,
        
        -- OutputState
        runOutputState :: !OutputState

    }




data RunEvent =
    EventLevelMode          |
    EventPuzzleMode         |
    EventMemoryMode         |
    EventForeign            |
    EventAbout              |
    EventSettings           |
    EventEggKonami



instance World RunWorld RunEvent where
    worldTick =
        worldTick . runGrid
    worldTickModify run f =
        run { runGrid = worldTickModify (runGrid run) f }
    worldAllEvents = 
        runEvents
    worldPushEvent run e =
        run { runEvents = runEvents run ++ [e] }


