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
module Game.Grid.Output.Fancy.ShadeSpace
  (
    shadeSpace,
    shadeSpaceColor,
    shadeSpaceWrite,

  ) where

import MyPrelude
import Game
import Game.Grid
import Game.Data.Color
import Game.Run.RunWorld.Scene

import OpenGL
import OpenGL.Helpers



shadeSpace :: ShadeSpace -> Tweak -> Float -> Mat4 -> IO ()
shadeSpace sh tweak alpha modv = do
    glUseProgram (shadeSpacePrg sh)
    glBindVertexArrayOES (shadeSpaceVAO sh)

    -- modv matrix
    uniformMat4 (shadeSpaceUniModvInvMatrix sh) $ mat4Transpose modv

    -- alpha
    glUniform1f (shadeSpaceUniAlpha sh) $ rTF alpha
    
    -- Tweak --
    glUniform4f (shadeSpaceUniDot sh) (1.0) (1.0) (1.0) (0.0)
    glUniform2f (shadeSpaceUniScalePlus sh) (1.0) (1.0)
    glUniform1f (shadeSpaceUniIntensity sh) (1.0)
    glUniform1f (shadeSpaceUniColorfy sh) (0.0)

    -- set textures
    glActiveTexture gl_TEXTURE0
    glBindTexture gl_TEXTURE_CUBE_MAP $ shadeSpaceTex sh

    -- draw
    glDrawArrays gl_TRIANGLE_STRIP 0 4

    



shadeSpaceColor :: ShadeSpace -> Tweak -> Float -> Mat4 -> Color -> IO ()
shadeSpaceColor sh tweak alpha modv color = do
    glUseProgram (shadeSpacePrg sh)
    glBindVertexArrayOES (shadeSpaceVAO sh)

    -- modv matrix
    uniformMat4 (shadeSpaceUniModvInvMatrix sh) $ mat4Transpose modv

    -- alpha
    glUniform1f (shadeSpaceUniAlpha sh) $ rTF alpha
    
    -- color
    uniformColor (shadeSpaceUniColor sh) color

    -- Tweak --
    glUniform4f (shadeSpaceUniDot sh) (1.0) (1.0) (1.0) (0.0)
    glUniform2f (shadeSpaceUniScalePlus sh) (1.0) (1.0)
    glUniform1f (shadeSpaceUniIntensity sh) (1.0)
    glUniform1f (shadeSpaceUniColorfy sh) (1.0)

    -- set textures
    glActiveTexture gl_TEXTURE0
    glBindTexture gl_TEXTURE_CUBE_MAP $ shadeSpaceTex sh

    -- draw
    glDrawArrays gl_TRIANGLE_STRIP 0 4



shadeSpaceWrite :: ShadeSpace -> Shape -> IO ()
shadeSpaceWrite sh (Shape wth hth) = do
    glProgramUniform2fEXT (shadeSpacePrg sh) (shadeSpaceUniScale sh) (rTF wth) (rTF hth)



