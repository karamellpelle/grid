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
module Game.Grid.Output.Plain.ShadeSpaceBox
  (
    shadeSpaceBox,
    shadeSpaceBoxColor,

  ) where

import MyPrelude
import Game
import Game.Grid
import Game.Data.Color

import OpenGL
import OpenGL.Helpers



shadeSpaceBox :: ShadeSpaceBox -> Float -> Mat4 -> Mat4 -> Float -> IO ()
shadeSpaceBox sh alpha projmodv proj radius = 
    shadeSpaceBoxColor sh alpha projmodv proj radius $ Color 0.0 0.0 0.0 1.0


shadeSpaceBoxColor :: ShadeSpaceBox -> Float -> Mat4 -> Mat4 -> Float -> Color -> IO ()
shadeSpaceBoxColor sh alpha projmodv proj radius color = do
    glUseProgram (shadeSpaceBoxPrg sh)
    glBindVertexArrayOES (shadeSpaceBoxVAO sh)

    -- projmodv matrix
    uniformMat4 (shadeSpaceBoxUniProjModvMatrix sh) projmodv

    -- PW
    case mat4W proj of
        (w0, w1, w2, w3) -> glUniform4f (shadeSpaceBoxUniPW sh) 
                                        (rTF w0) (rTF w1) (rTF w2) (rTF w3)

    -- alpha
    glUniform1f (shadeSpaceBoxUniAlpha sh) $ rTF alpha

    glDisable gl_DEPTH_TEST

    -- set textures
    glActiveTexture gl_TEXTURE0
    glBindTexture gl_TEXTURE_CUBE_MAP $ shadeSpaceBoxTex sh

    -- radius
    glUniform1f (shadeSpaceBoxUniRadius sh) $ rTF radius

    -- color
    uniformColor (shadeSpaceBoxUniColor sh) color

    -- draw
    glDrawArrays gl_TRIANGLE_STRIP 0 (8 + 2 + 8)

    glEnable gl_DEPTH_TEST
