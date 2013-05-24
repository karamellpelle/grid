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
module Game.Run.Do.Grid.Modify
  (
    controlRunCamera,

  ) where


import MyPrelude
import Game

import Game.Grid


-- | control camera from input
controlRunCamera :: s -> GridWorld -> b -> MEnv' (s, GridWorld, b)
controlRunCamera = \s grid b -> do
    grid' <- inputCamera grid
    return (s, grid', b)



--------------------------------------------------------------------------------
-- 

inputCamera :: GridWorld -> MEnv' GridWorld
inputCamera grid = do
    gridModifyCameraM grid $ \camera -> do
        -- save current view
        camera' <- keysTouchHandleCircleTouched camera $ \_ _ ->
            let View a b c = cameraCurrentView camera
            in  camera
                {
                    -- current view
                    cameraView = View a b c,    

                    cameraViewIdeal = View (a + 1.0) (b + 1.0) (c + 1.0),
                    cameraViewAAlpha = 0.0,
                    cameraViewAAlphaIdeal = 0.0,
                    cameraViewASpeed = valueGridCameraViewASpeed,
                    cameraViewBAlpha = 0.0,
                    cameraViewBAlphaIdeal = 0.0,
                    cameraViewBSpeed = valueGridCameraViewBSpeed,
                    cameraViewCAlpha = 0.0,
                    cameraViewCAlphaIdeal = 0.0,
                    cameraViewCSpeed = cscale * valueGridCameraViewCSpeed
                }

        -- move camera
        keysTouchHandleCircleDrag camera' $ \ticks (x, y) r (x', y') r' -> 
            let View a b c = cameraView camera'
                aAlphaIdeal = (x' - x) * valueGridCameraViewASens
                bAlphaIdeal = keepInside (bMin - b) (bMax - b) $
                                         (y' - y) * valueGridCameraViewBSens
                cAlphaIdeal = keepInside 
                              (valueRunGridCameraViewCMin - c)
                              (valueRunGridCameraViewCMax - c)
                              (cscale * (r - r') * valueGridCameraViewCSens)
            in  camera'
                {
                    cameraViewAAlphaIdeal = aAlphaIdeal,
                    cameraViewBAlphaIdeal = bAlphaIdeal, 
                    cameraViewCAlphaIdeal = cAlphaIdeal
                }
   
    where
      gridModifyCameraM grid f = do
          cam' <- f $ gridCamera grid
          return grid { gridCamera = cam' }

      bMin = (-0.249 * tau)
      bMax = 0.249 * tau
      cscale = valueRunGridCameraViewCScale 
