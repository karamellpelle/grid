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
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-} 

module Game.Grid.GridWorld
  (
    GridWorld (..),
    GridEvent (..),
    
    module Game.Grid.GridWorld.Path,
    module Game.Grid.GridWorld.Camera,
    module Game.Grid.GridWorld.Segment,
    module Game.Grid.GridWorld.CameraCommand,

  ) where


import MyPrelude
import Game.MEnv
import Game.World
import Game.Grid.GridWorld.Turn
import Game.Grid.GridWorld.Path
import Game.Grid.GridWorld.Camera
import Game.Grid.GridWorld.Node
import Game.Grid.GridWorld.Segment
import Game.Grid.GridWorld.CameraCommand



data GridWorld =
    GridWorld
    {
        gridTick :: !TickT,
        gridEvents :: [GridEvent],

        gridCamera :: !Camera,

        gridCameraCommands :: ![CameraCommand],
        gridCameraCommandTick :: !TickT,

        -- physical objects:
        -- fixme: use either gridPaths, or gridPathA,
        -- if we use some array-container, we can probably let PathIx be a direct index,
        -- not causing so much overhead as searching for ix each time (an old functional
        -- programmer probably now the best datatype...)
        --gridPaths :: [Path]
        gridPathA :: !Path,

        -- control state
        gridControlPosRef :: Position

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

