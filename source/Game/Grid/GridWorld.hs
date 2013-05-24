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
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
module Game.Grid.GridWorld
  (
    GridWorld (..),
    GridEvent (..),
    
    module Game.Grid.GridWorld.Camera,
    module Game.Grid.GridWorld.CameraCommand,
    module Game.Grid.GridWorld.Path,
    module Game.Grid.GridWorld.SegmentArray,
    module Game.Grid.GridWorld.Segment,
    module Game.Grid.GridWorld.Node,
    module Game.Grid.GridWorld.Turn,

  ) where


import MyPrelude
import Game
import Game.Grid.GridWorld.Camera
import Game.Grid.GridWorld.CameraCommand
import Game.Grid.GridWorld.Path
import Game.Grid.GridWorld.SegmentArray
import Game.Grid.GridWorld.Segment
import Game.Grid.GridWorld.Node
import Game.Grid.GridWorld.Turn



data GridWorld =
    GridWorld
    {
        gridTick :: !Tick,
        gridEvents :: [GridEvent],

        gridCamera :: !Camera,

        gridCameraCommands :: ![CameraCommand],
        gridCameraCommandTick :: !Tick,
        gridCameraCommandScale :: !Float,
        gridCameraCommandCount :: !UInt,

        -- physical objects:
        --gridPaths :: [Path]
        gridPath :: !Path


    } 

--------------------------------------------------------------------------------
--  


-- | GridEvent
data GridEvent
    --EventCameraCommandsComplete


instance World GridWorld GridEvent where
    worldTick = 
        gridTick
    worldTickModify grid f =
        grid { gridTick = f $ gridTick grid }
    worldAllEvents =
        gridEvents
    worldPushEvent grid e =
        grid { gridEvents = gridEvents grid ++ [e] }



--------------------------------------------------------------------------------
--  

