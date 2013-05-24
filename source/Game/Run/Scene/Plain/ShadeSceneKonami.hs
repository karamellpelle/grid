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
module Game.Run.Scene.Plain.ShadeSceneKonami
  (
    shadeSceneKonami,

  ) where

import MyPrelude
import Game
import Game.Run.RunWorld
import Game.Run.RunData

import OpenGL
import OpenGL.Helpers





shadeSceneKonami :: ShadeSceneKonami -> Float -> Mat4 -> Float -> Scene -> IO ()
shadeSceneKonami sh alpha projmodv tweak scene = do

    glUseProgram $ shadeSceneKonamiPrg sh

    -- projmodv
    uniformMat4 (shadeSceneKonamiUniProjModvMatrix sh) projmodv
    
    -- alpha 
    glUniform1f (shadeSceneKonamiUniAlpha sh) $ rTF alpha

    -- tweak
    glUniform1f (shadeSceneKonamiUniTweak sh) $ rTF tweak

    glDisable gl_DEPTH_TEST

    glActiveTexture gl_TEXTURE0
    glBindTexture gl_TEXTURE_2D $ sceneTex scene 
    glActiveTexture gl_TEXTURE1
    glBindTexture gl_TEXTURE_2D $ shadeSceneKonamiTex sh

    glBindVertexArrayOES $ shadeSceneKonamiVAO sh
    glDrawArrays gl_TRIANGLE_STRIP 0 4
    
    -- enable depth?



