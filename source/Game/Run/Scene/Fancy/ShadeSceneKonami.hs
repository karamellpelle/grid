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
module Game.Run.Scene.Fancy.ShadeSceneKonami
  (
    shadeSceneKonamiTweak,

  ) where

import MyPrelude
import Game
import Game.Run

import OpenGL
import OpenGL.Helpers





shadeSceneKonamiTweak :: SceneData -> ShadeSceneKonami -> Tweak -> RunWorld -> Scene -> IO ()
shadeSceneKonamiTweak sdata sh tweak run scene = do

    glUseProgram $ shadeSceneKonamiPrg sh
    
    -- shape of scene
    let Shape wth hth = sceneShape scene
    glUniform2f (shadeSceneKonamiUniStencilScale sh) (rTF $ 1.0 / hth) (rTF $ 1.0 / wth)
    
    let tick = gridTick $ runGrid run
        tick' = gridCameraCommandTick $ runGrid run
        scale = gridCameraCommandScale $ runGrid run
    
    -- alphas
    let (stencilA, sceneA) = case (gridCameraCmdsCount $ runGrid run) of
                             1     -> (cmdAlpha run, 0.0)
                             2     -> (1.0, 0.0)
                             3     -> (1.0, cmdAlpha run)
                             _     -> (0.0, 1.0)

    glUniform1f (shadeSceneKonamiUniStencilAlpha sh) (rTF stencilA)
    glUniform1f (shadeSceneKonamiUniSceneAlpha sh) (rTF sceneA)
        
    -- Tweak uniforms --
    let alphaX = abs $ tweakOsc0Pos tweak
        alphaY = abs $ tweakOsc1Pos tweak
    glUniform4f (shadeSceneKonamiUniFillColor sh) (1) (1) (1) (1)
    glUniform1f (shadeSceneKonamiUniXAlpha sh) (rTF alphaX)
    glUniform4f (shadeSceneKonamiUniXColor sh) (0) (1) (0) (1)
    glUniform1f (shadeSceneKonamiUniYAlpha sh) (rTF alphaY)
    glUniform4f (shadeSceneKonamiUniYColor sh) (0) (0) (1) (1)

    -- textures 
    glActiveTexture gl_TEXTURE0
    glBindTexture gl_TEXTURE_2D $ sceneTex scene 
    glActiveTexture gl_TEXTURE1
    glBindTexture gl_TEXTURE_2D $ shadeSceneKonamiStencil sh

    -- draw --
    glBindVertexArrayOES $ scenedataVAO sdata
    glDrawArrays gl_TRIANGLE_STRIP 0 4
    
    where
      cmdAlpha run = 
          let tick = gridTick $ runGrid run
              tick' = gridCameraCommandTick $ runGrid run
              scale = gridCameraCommandScale $ runGrid run

          in  1.0 - (rTF (tick' - tick) * scale)
