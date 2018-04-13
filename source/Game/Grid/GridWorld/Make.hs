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
module Game.Grid.GridWorld.Make
  (
    makeGridWorld,
    makeGridWorldCamera,
    makeGridWorldCameraView,
    makeGridWorldView,
    makeGridWorldEmpty,
    destroyGridWorld,

  ) where


import MyPrelude
import Game.MEnv
import Game.Grid.GridWorld



--------------------------------------------------------------------------------
--  


-- | make trivial GridWorld
makeGridWorldEmpty :: MEnv' GridWorld
makeGridWorldEmpty = 
    makeGridWorld 0


-- | make GridWorld
makeGridWorld :: UInt -> MEnv' GridWorld
makeGridWorld pathsize =
    makeGridWorldCamera pathsize makeCamera


-- | make GridWorld, copy camera
makeGridWorldCamera :: UInt -> Camera -> MEnv' GridWorld
makeGridWorldCamera pathsize camera = do
    path <- makePath pathsize
    return  GridWorld 
            { 
                gridTick = 0.0,
                gridEvents = [],
                gridPath = path,

                gridCamera = camera,

                gridCameraCommands = [],
                gridCameraCommandTick = 0.0,
                gridCameraCommandScale = 0.0,
                gridCameraCommandCount = 0
            }


-- | make GridWorld, copy camera view
makeGridWorldCameraView :: UInt -> Camera -> MEnv' GridWorld
makeGridWorldCameraView pathsize camera =
    makeGridWorldCamera pathsize $ makeCameraCameraView camera 


-- | make GridWorld, with camera view
makeGridWorldView :: UInt -> View -> MEnv' GridWorld
makeGridWorldView pathsize view =
    makeGridWorldCamera pathsize $  makeCameraView view


-- | clean up
destroyGridWorld :: GridWorld -> MEnv' ()
destroyGridWorld grid = do
    destroyPath $ gridPath grid


