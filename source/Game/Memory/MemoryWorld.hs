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
{-# LANGUAGE MultiParamTypeClasses #-}
module Game.Memory.MemoryWorld
  (
    MemoryWorld (..),
    MemoryEvent (..),

    module Game.Grid,

  ) where


import MyPrelude
import Game

import Game.Grid
import Game.Memory.MemoryWorld.OutputState


data MemoryWorld = 
    MemoryWorld
    {
        -- level
        memoryLevelIx :: !UInt,
        memoryLevelSize :: !UInt,
        memoryLevelCurrent :: [Turn],
        memoryLevelNexts :: [ [Turn] ],

        -- state
        memoryGrid :: !GridWorld,
        memoryFood :: [Turn],
        memoryEvents :: [MemoryEvent],
        memorySpaceRef :: !Turn,

        -- failure?
        memoryIsFailure :: !Bool,
        memoryFailureIx :: !UInt,
        memoryFailureSegment :: !Segment,
        memoryFailureTick :: !Tick,

        -- OutputState
        memoryOutputState :: !OutputState
    }



data MemoryEvent =
    EventAllFoodEaten      |  -- ^ all food eaten
    EventWrongTurn            -- ^ failure at some index

instance World MemoryWorld MemoryEvent where
    worldTick = 
        worldTick . memoryGrid
    worldTickModify mem f =
        mem { memoryGrid = worldTickModify (memoryGrid mem) f } -- fixme
    worldAllEvents =
        memoryEvents
    worldPushEvent mem e =
        mem { memoryEvents = memoryEvents mem ++ [e] }

