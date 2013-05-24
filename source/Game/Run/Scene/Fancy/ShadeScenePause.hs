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
module Game.Run.Scene.Fancy.ShadeScenePause
  (
    shadeScenePauseTweak,

  ) where

import MyPrelude
import Game
import Game.Run.RunWorld
import Game.Run.RunData

import OpenGL
import OpenGL.Helpers





shadeScenePauseTweak :: SceneData -> ShadeScenePause -> Tweak -> Scene -> IO ()
shadeScenePauseTweak sdata sh tweak scene = do

    glUseProgram $ shadeScenePausePrg sh
    glDisable gl_DEPTH_TEST



    -- set Tweak uniforms --
    --glUniform1f (shadeScenePauseUni sh) (rTF )
    

    -- textures --
    glActiveTexture gl_TEXTURE0
    glBindTexture gl_TEXTURE_2D $ sceneTex scene 
    glActiveTexture gl_TEXTURE1
    glBindTexture gl_TEXTURE_2D $ scenedataTexA sdata 

    -- draw --
    glBindVertexArrayOES $ scenedataVAO sdata
    glDrawArrays gl_TRIANGLE_STRIP 0 4
    

