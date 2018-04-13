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
module Game.Grid.Helpers.Camera
  (
    cameraEatCameraCommand,

    cameraToPath,
    cameraToPathTranslate,
    cameraToPathTurn,
    cameraSetPath,

    cameraCurrentView,
    cameraViewAlpha,
    cameraToView,
    cameraToViewTicks,
    cameraToView',
    cameraToViewTicks',
    cameraSetView,
    cameraIsAtView,

    cameraTurn,
    cameraTurnIdeal,
    cameraToTurn,
    cameraToTurnTicks,
    cameraSetTurn,
    cameraIsAtTurn,
  
    cameraToTurnView,
    cameraSetTurnView,

  ) where

import MyPrelude

import Game
import Game.Grid.GridWorld
import Game.Grid.Helpers.Path



--------------------------------------------------------------------------------
--  

cameraEatCameraCommand :: Camera -> CameraCommand -> Camera
cameraEatCameraCommand cam cmd =
    let turn = if cameracommandTurnIsAdd cmd
               then cameracommandTurn cmd `mappend` cameraTurnIdeal cam
               else cameracommandTurn cmd
        view = if cameracommandViewIsAdd cmd
               then cameracommandView cmd `mappend` cameraCurrentView cam
               else cameracommandView cmd
    in  let cam' = cameraToTurn cam (cameracommandTurnSpeed cmd) turn 
        in  if cameracommandViewIsAdd cmd 
            then cameraToView' cam' (cameracommandViewSpeed cmd) view
            else cameraToView cam' (cameracommandViewSpeed cmd) view



--------------------------------------------------------------------------------
--  Camera + Path


-- | follow path by translation and turning
--   fixme: see StepDT
cameraToPath :: Camera -> Path -> Camera 
cameraToPath camera path =
    cameraToPathTurn (cameraToPathTranslate camera path) path 


-- | follow path by translation
--   fixme: see StepDT
cameraToPathTranslate :: Camera -> Path -> Camera
cameraToPathTranslate camera path =
    let node' = if pathWaiting path 
                then pathNode path 
                else pathNodeNext path
    in  if cameraNodeIdeal camera == node'
        then camera 
        else camera
             { 
                --cameraNode = cameraNodeIdeal camera,
                cameraNodeIdeal = node',
                cameraNodeAlpha = pathAlpha path,
                cameraNodeSpeed = pathSpeed path 
             }


-- | follow path by turning
--   fixme: see StepDT
cameraToPathTurn :: Camera -> Path -> Camera 
cameraToPathTurn camera path = 
    let turn = cameraTurnIdeal camera
        turn' = pathTurn path
    in if turn == turn'
       then camera
       else let b = cameraTurnB camera
                b' = cameraTurnBIdeal camera
                balpha = cameraTurnBAlpha camera
                a = cameraTurnA camera
                a' = cameraTurnAIdeal camera
                aalpha = cameraTurnAAlpha camera

            in  camera
                {
                    cameraTurnBIdeal = turn',
                    cameraTurnB = b',
                    cameraTurnBAlpha = 0.0, -- if pathAlpha path, then discontiuos roll!
                    cameraTurnASpeed = pathSpeed path * valueGridPathTurnSpeed,
                    cameraTurnBSpeed = pathSpeed path * valueGridPathTurnSpeed,
                    cameraTurnAIdeal = mempty,
                    cameraTurnA = turnInverse b' `mappend` b,
                    cameraTurnAAlpha = balpha
                }



-- | set camera to path position and orientation
cameraSetPath :: Camera -> Path -> Camera
cameraSetPath camera path =
    camera 
    {
        cameraNode = pathNode path,
        cameraNodeIdeal = pathNodeNext path,
        cameraNodeAlpha = pathAlpha path,
        cameraNodeSpeed = if pathWaiting path then 0.0 else pathSpeed path,
        cameraTurnB = pathTurn path,
        cameraTurnBIdeal = pathTurn path,
        cameraTurnBAlpha = 1.0,
        cameraTurnBSpeed = valueGridPathTurnSpeed * pathSpeed path, -- ??
        cameraTurnA = mempty,
        cameraTurnAIdeal = mempty,
        cameraTurnAAlpha = 1.0,
        cameraTurnASpeed = valueGridPathTurnSpeed * pathSpeed path  -- ??
    }


--------------------------------------------------------------------------------
--  Camera + Turn


cameraTurn :: Camera -> Turn
cameraTurn cam = 
    cameraTurnB cam `mappend` cameraTurnA cam


cameraTurnIdeal :: Camera -> Turn
cameraTurnIdeal cam =
    cameraTurnBIdeal cam `mappend` cameraTurnAIdeal cam 



cameraSetTurn :: Camera -> Turn -> Camera
cameraSetTurn camera turn =
    camera 
    {
        cameraTurnBIdeal = turn,
        cameraTurnB = turn,
        cameraTurnAIdeal = mempty,
        cameraTurnA = mempty
    }


cameraIsAtTurn :: Camera -> Bool
cameraIsAtTurn camera =
    1.0 <= (cameraTurnAAlpha camera) &&
    1.0 <= (cameraTurnBAlpha camera)


cameraToTurnTicks :: Camera -> Float -> Turn -> Camera
cameraToTurnTicks camera ticks turn =
    cameraToTurn camera (1.0 / ticks) turn


cameraToTurn :: Camera -> Float -> Turn -> Camera
cameraToTurn camera speed turn =
    let b = cameraTurnB camera
        b' = cameraTurnBIdeal camera
        balpha = cameraTurnBAlpha camera
        a = cameraTurnA camera
        a' = cameraTurnAIdeal camera
        aalpha = cameraTurnAAlpha camera
    in  camera 
        {
            cameraTurnBIdeal = turn,
            cameraTurnB = b',
            cameraTurnBAlpha = 0.0,
            cameraTurnBSpeed = speed,
            cameraTurnAIdeal = mempty,
            cameraTurnA = turnInverse b' `mappend` b,
            cameraTurnAAlpha = balpha,
            cameraTurnASpeed = speed
        }


--------------------------------------------------------------------------------
--  Camera + View

cameraViewAlpha :: Camera -> Float
cameraViewAlpha camera =
    0.33333333 * (cameraViewAAlpha camera + 
                  cameraViewBAlpha camera + 
                  cameraViewCAlpha camera)


cameraCurrentView :: Camera -> View
cameraCurrentView camera =
    let View a b c = cameraView camera
        View a' b' c' = cameraViewIdeal camera
        a0 = smooth a a' $ cameraViewAAlpha camera
        b0 = smooth b b' $ cameraViewBAlpha camera
        c0 = smooth c c' $ cameraViewCAlpha camera
    in  View a0 b0 c0


cameraToView :: Camera -> Float -> View -> Camera
cameraToView camera speed (View a1 b1 c1) =
    case cameraCurrentView camera of
        View a0 b0 c0   -> 
            cameraToView' camera speed $ View (shortest a0 a1)
                                              (shortest b0 b1)
                                              c1
    where
      shortest x x' =
          let pos = modulo x (x + tau) x'
              neg = modulo x' (x' + tau) x
          in  if pos <= neg
              then x + pos 
              else x - neg


cameraToViewTicks :: Camera -> Float -> View -> Camera
cameraToViewTicks camera ticks view = 
    cameraToView camera (1.0 / ticks) view


-- | to view strictly
cameraToView' :: Camera -> Float -> View -> Camera
cameraToView' camera speed view = 
    camera 
    { 
        cameraView = cameraCurrentView camera,
        cameraViewIdeal = view, 
        cameraViewAAlpha = 0.0,
        cameraViewAAlphaIdeal = 1.0,
        cameraViewASpeed = speed,
        cameraViewBAlpha = 0.0,
        cameraViewBAlphaIdeal = 1.0,
        cameraViewBSpeed = speed,
        cameraViewCAlpha = 0.0,
        cameraViewCAlphaIdeal = 1.0,
        cameraViewCSpeed = speed
    }

cameraSetView :: Camera -> View -> Camera
cameraSetView camera view =
    camera
    {
        cameraView = view,
        cameraViewIdeal = view
    }


cameraToViewTicks' :: Camera -> Tick -> View -> Camera
cameraToViewTicks' camera ticks view = 
    cameraToView' camera (1.0 / rTF ticks) view




cameraIsAtView :: Camera -> Bool
cameraIsAtView camera = 
    cameraViewAAlpha camera == cameraViewAAlphaIdeal camera &&
    cameraViewBAlpha camera == cameraViewBAlphaIdeal camera &&
    cameraViewCAlpha camera == cameraViewCAlphaIdeal camera 



--------------------------------------------------------------------------------
--  

cameraToTurnView :: Camera -> Float -> Turn -> View -> Camera
cameraToTurnView cam speed turn view =
    cameraToView (cameraToTurn cam speed turn) speed view


cameraSetTurnView :: Camera -> Turn -> View -> Camera
cameraSetTurnView camera turn view =
    cameraSetView (cameraSetTurn camera turn) view


--------------------------------------------------------------------------------
--  


cameraToNode :: Camera -> Float -> Node -> Camera
cameraToNode camera speed node = 
    camera 
    {
      cameraNode = cameraNodeIdeal camera,
      cameraNodeIdeal = node,
      cameraNodeAlpha = 0.0,
      cameraNodeSpeed = speed
    }

cameraSetNode :: Camera -> Node -> Camera
cameraSetNode camera node =
    camera
    {
        cameraNode = node,
        cameraNodeIdeal = node,
        cameraNodeAlpha = 0.0
    }



cameraToNodeTicks :: Camera -> Float -> Node -> Camera
cameraToNodeTicks camera ticks node =
    cameraToNode camera (1.0 / ticks) node


cameraIsAtNode :: Camera -> Bool
cameraIsAtNode camera =
    1.0 <= cameraNodeAlpha camera


