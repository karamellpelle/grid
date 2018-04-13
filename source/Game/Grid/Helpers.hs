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
module Game.Grid.Helpers
  (
    gridModifyTick,
    gridModifyCamera,
    gridClearEvents,
    gridIsEmpty,

    gridCameraCmdsIsComplete,
    gridCameraCmdsCount,
    gridPushCameraCmds,
    gridPushCameraCmd,
    gridSetCameraCmds,
    gridClearCameraCmds,

    turnFromDir,
    dirNodeNode,
    turnMultNode,

    module Game.Grid.Helpers.Path,
    module Game.Grid.Helpers.Segment,
    module Game.Grid.Helpers.Camera,

  ) where

import MyPrelude
import Game
import Game.Grid.GridWorld
import Game.Grid.Helpers.Path
import Game.Grid.Helpers.Segment
import Game.Grid.Helpers.Camera



--------------------------------------------------------------------------------
--  

gridClearEvents :: GridWorld -> GridWorld
gridClearEvents grid =
    grid
    {
        gridPath = pathClearEvents (gridPath grid),
        gridEvents = []
    }

gridIsEmpty :: GridWorld -> Bool
gridIsEmpty grid =
    pathArraySize (gridPath grid) == 0


--------------------------------------------------------------------------------
--  

gridModifyCamera :: GridWorld -> (Camera -> Camera) -> GridWorld
gridModifyCamera grid f =
    grid { gridCamera = f (gridCamera grid) }


gridModifyTick :: GridWorld -> (Tick -> Tick) -> GridWorld
gridModifyTick grid f =
    grid { gridTick = f (gridTick grid) }





--------------------------------------------------------------------------------
--  CameraCommand

-- fixme: also GridEvent?
gridCameraCmdsIsComplete :: GridWorld -> Bool
gridCameraCmdsIsComplete grid =
    (null $ gridCameraCommands grid) && (gridCameraCommandTick grid <= worldTick grid)


gridCameraCmdsCount :: GridWorld -> UInt
gridCameraCmdsCount grid = 
    gridCameraCommandCount grid


gridPushCameraCmd :: GridWorld -> CameraCommand -> GridWorld
gridPushCameraCmd grid cmd = 
    gridPushCameraCmds grid [cmd]


gridPushCameraCmds :: GridWorld -> [CameraCommand] -> GridWorld
gridPushCameraCmds grid cmds =
    gridSetCameraCmds grid $ gridCameraCommands grid ++ cmds


gridSetCameraCmds :: GridWorld -> [CameraCommand] -> GridWorld
gridSetCameraCmds grid cmds =
    grid
    {
        gridCameraCommands = cmds,
        gridCameraCommandCount = 0
    }


gridClearCameraCmds :: GridWorld -> GridWorld
gridClearCameraCmds grid = 
    gridSetCameraCmds grid []




--------------------------------------------------------------------------------
--  

dirNodeNode :: Node -> Node -> Dir
dirNodeNode node node' =
    case nodeDiff node node' of
        Node x y z -> Dir (fI $ signum x) (fI $ signum y) (fI $ signum z)


turnMultNode :: Turn -> Node -> Node
turnMultNode (Turn a0 a1 a2 b0 b1 b2 c0 c1 c2) (Node n0 n1 n2) =
    Node (n0 * fI a0 + n1 * fI b0 + n2 * fI c0)
         (n0 * fI a1 + n1 * fI b1 + n2 * fI c1)
         (n0 * fI a2 + n1 * fI b2 + n2 * fI c2)


turnFromDir :: Turn -> Dir -> Turn
turnFromDir turn dir =
    let diff = case helper (turnInverse turn) dir of
                  Dir 1 0 0     -> straightTurn
                  Dir 0 1 0     -> upTurn
                  Dir 0 0 1     -> rightTurn
                  Dir (-1) 0 0  -> backTurn
                  Dir 0 (-1) 0  -> downTurn
                  Dir 0 0 (-1)  -> leftTurn
                  Dir 0 0 0     -> straightTurn
                  _             -> error "turnFromDir: no such direction" 

    in diff `mappend` turn

    where 
      helper (Turn x0 x1 x2
                   y0 y1 y2
                   z0 z1 z2)
             (Dir d0 d1 d2) =
        Dir  (x0 * d0 + y0 * d1 + z0 * d2)
             (x1 * d0 + y1 * d1 + z1 * d2)
             (x2 * d0 + y2 * d1 + z2 * d2)
