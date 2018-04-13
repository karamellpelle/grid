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
module Game.Run.Scene.Fancy
  (
    sceneBegin,
    sceneHandleEscape,

    module Game.Run.Scene.Fancy.Present,
    module Game.Run.Scene.Fancy.Step,

  ) where

import MyPrelude
import Game

import Game.Grid
import Game.Run

import Game.Grid.Output.Fancy.ShadeGeneral
import Game.Run.Scene.Fancy.Present
import Game.Run.Scene.Fancy.Step
import Game.Run.Scene.Fancy.ShadeScreenshot

import OpenGL
import OpenGL.Helpers



-- | begin Scene
sceneBegin :: Scene -> MEnv' Scene
sceneBegin scene = io $ do

    -- render to sceneTex
    glBindFramebuffer gl_FRAMEBUFFER $ sceneFBO scene
    glClear $ gl_COLOR_BUFFER_BIT .|.  gl_DEPTH_BUFFER_BIT .|.  gl_STENCIL_BUFFER_BIT

    return scene
                


-- | escape from Scene
sceneHandleEscape :: Scene -> MEnv' a -> MEnv' a -> MEnv' a
sceneHandleEscape scene ma ma' = do
    join $ keysTouchHandleButtonA ({-drawCorners2D >> -}ma) $ \(x, y) -> 
           let Shape wth hth = sceneShape scene
           in  if inTriangle x y                  || 
                  inTriangle (wth - x) y          || 
                  inTriangle (wth - x) (hth - y)  ||
                  inTriangle x (hth - y)
               then ma'
               else {-drawCorners2D >> -}ma

    where
      inTriangle x y =
          let r = valueSceneCornerSize
              s = valueSceneCornerSize
          in  r * y + s * x < s * r
      
      drawCorners2D = do
          gamedata <- resourceGameData
          io $ do
              let sh = griddataShadeGeneral $ gamedataGridData gamedata
                  cornerdata = rundataCornerData $ gamedataRunData gamedata
              shadeGeneral sh 1.0 $ sceneProj2D scene
             
              glActiveTexture gl_TEXTURE0
              glBindTexture gl_TEXTURE_2D $ cornerdataTex cornerdata

              -- draw
              glDisable gl_CULL_FACE
              glBindVertexArrayOES $ cornerdataVAO2D cornerdata
              glDrawArrays gl_TRIANGLE_STRIP 0 24
              glEnable gl_CULL_FACE

