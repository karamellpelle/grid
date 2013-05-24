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
module Game.Grid.GridWorld.Camera
  (
      Camera (..),

      makeCamera,
      makeCameraWithView,
      makeCameraWithCameraView,

      module Game.Data.View,

  ) where


import Game.Grid.GridWorld.Turn
import Game.Grid.GridWorld.Node
import Game.Data.View


-- note that the camera is just a physical object of the world, and an 
-- output-function of world may choose to use that object or not when drawing 
-- the world.
-- 
-- the idea of TurnA, TurnB, TurnC, ... is to handle multiple turns T0, T1, T2, 
-- ... as a telescopic product (T2 * T1^-1) * (T1 * T0^-1) in some way, to make 
-- camera move more continuous. this idea might also be used to handle 
-- interpolation of turns T0, T1 where an axis W is sendt to -W. such 
-- interpolations are currently not continuous. the same could be done with nodes 
-- and views, actually for any group G:
--
-- A' = I
-- A = (inv B') * B
-- alphaA = alphaB
-- B' = I
-- B = (inv C') * C'
-- alphaB = alphaC
-- C' = T
-- C = C'
-- alphaC = 0.0
--
-- for A, B, C
-- 
-- the View uses AlphaIdeal, and has such for each component. when updating 
-- Alpha in StepDT, Alpha moves against AlphaIdeal. AlphaIdeal's should also
-- be implemented for Node and Turn's. I have not done this (yet)
--

data Camera =
    Camera
    {
        -- translate
        cameraNode :: !Node,
        cameraNodeIdeal :: !Node,
        cameraNodeAlpha :: !Float,
        cameraNodeSpeed :: !Float,

        -- turn
        cameraTurnA :: !Turn,
        cameraTurnAIdeal :: !Turn,
        cameraTurnAAlpha :: !Float,
        cameraTurnASpeed :: !Float,

        cameraTurnB :: !Turn,
        cameraTurnBIdeal :: !Turn,
        cameraTurnBAlpha :: !Float,
        cameraTurnBSpeed :: !Float,

        -- view
        cameraView :: !View,
        cameraViewIdeal :: !View,
        cameraViewAAlpha :: !Float,
        cameraViewAAlphaIdeal :: !Float,
        cameraViewASpeed :: !Float,
        cameraViewBAlpha :: !Float,
        cameraViewBAlphaIdeal :: !Float,
        cameraViewBSpeed :: !Float,
        cameraViewCAlpha :: !Float,
        cameraViewCAlphaIdeal :: !Float,
        cameraViewCSpeed :: !Float




    }



-- | make new camera
makeCamera :: Camera
makeCamera =
    Camera
    { 
        cameraNode = Node 0 0 0,
        cameraNodeIdeal = Node 0 0 0,
        cameraNodeAlpha = 1.0,
        cameraNodeSpeed = 1.0,

        cameraTurnA = straightTurn,
        cameraTurnAIdeal = straightTurn,
        cameraTurnAAlpha = 1.0,
        cameraTurnASpeed = 1.0,

        cameraTurnB = straightTurn,
        cameraTurnBIdeal = straightTurn,
        cameraTurnBAlpha = 1.0,
        cameraTurnBSpeed = 1.0,

        cameraView = View 0.0 0.0 1.0,
        cameraViewIdeal = View 0.0 0.0 1.0,
        cameraViewAAlpha = 0.0,
        cameraViewAAlphaIdeal = 1.0,
        cameraViewASpeed = 1.0,
        cameraViewBAlpha = 0.0,
        cameraViewBAlphaIdeal = 1.0,
        cameraViewBSpeed = 1.0,
        cameraViewCAlpha = 0.0,
        cameraViewCAlphaIdeal = 1.0,
        cameraViewCSpeed = 1.0
    }                                          


-- | make new camera, with View-part from 'camera'
makeCameraWithCameraView :: Camera -> Camera
makeCameraWithCameraView camera = 
    makeCamera
    {
        cameraView = cameraView camera,
        cameraViewIdeal = cameraViewIdeal camera,
        cameraViewAAlpha = cameraViewAAlpha camera,
        cameraViewAAlphaIdeal = cameraViewAAlphaIdeal camera,
        cameraViewASpeed = cameraViewASpeed camera,
        cameraViewBAlpha = cameraViewBAlpha camera,
        cameraViewBAlphaIdeal = cameraViewBAlphaIdeal camera,
        cameraViewBSpeed = cameraViewBSpeed camera,
        cameraViewCAlpha = cameraViewCAlpha camera,
        cameraViewCAlphaIdeal = cameraViewCAlphaIdeal camera, 
        cameraViewCSpeed = cameraViewCSpeed camera
    }


makeCameraWithView :: View -> Camera
makeCameraWithView view = 
    makeCamera
    {
        cameraView = view,
        cameraViewIdeal = view 
    }

